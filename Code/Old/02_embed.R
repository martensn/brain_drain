Sys.setenv(RETICULATE_PYTHON = "/usr/local/bin/python3")
library(readxl)
library(tigris)
library(reticulate)
library(matrixStats)
library(reticulate)
library(stringi)
library(openai)
library(RANN)
library(tidyr)
library(tidyverse)
library(table.express)
library(data.table)


library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
data_dir = file.path(directory,"Data")

# Not relevant for anything else in step 2, but necessary for step 4
# relies on a tigris which cannot be installed on the cluster
state_fips = fips_codes %>% 
  select(state,state_code) %>% 
  distinct() %>%
  mutate(state_fips = as.numeric(state_code)) %>%
  select(state,state_fips) %>%
  rename(state_abbr = state)
saveRDS(state_fips,file.path(data_dir,"intermediate/state_fips.rds"))

# Minimum similarity score of top match 
hs_threshold = 0.91
col_threshold = 0.83

# Function to convert large vectors to embeddings without overwhelming OpenAI
batch_embed <- function(strings, model, batch_size = 1000) {
  strings <- strings |> as.character()
  strings <- strings[!is.na(strings) & strings != ""]
  batches <- split(strings, ceiling(seq_along(strings) / batch_size))
  out <- vector("list", length(batches))
  
  for (i in seq_along(batches)) {
    message("Embedding batch ", i, " / ", length(batches))
    
    resp <- create_embedding(
      model = model,
      input = batches[[i]]
    )
    
    # THIS is the crucial part
    out[[i]] <- data.frame(
      input = batches[[i]],
      embedding = I(resp$data$embedding),
      stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

cosine_sim <- function(x, Y) {
  # x: numeric vector (query embedding)
  # Y: matrix where rows = reference embeddings
  drop((Y %*% x) / (sqrt(rowSums(Y^2)) * sqrt(sum(x^2))))
}

faiss <- import("faiss")
np    <- import("numpy")

build_faiss_index <- function(alias_matrix) {
  normalize <- function(x) x / sqrt(rowSums(x^2))
  
  A <- normalize(alias_matrix)
  A_np <- np$array(A, dtype = "float32")
  
  d <- ncol(A_np)
  index <- faiss$IndexFlatIP(d)   # exact cosine via inner product
  index$add(A_np)
  
  index
}

top_embed <- function(raw_resp, alias_resp, index, n = 5) {
  
  normalize <- function(x) x / sqrt(rowSums(x^2))
  
  Q <- normalize(do.call(rbind, raw_resp$embedding))
  Q_np <- np$array(Q, dtype = "float32")
  
  res <- index$search(Q_np, as.integer(n))
  
  scores <- res[[1]]   # similarities
  idx    <- res[[2]]   # indices into alias table (0-based)
  
  bind_rows(lapply(seq_len(nrow(Q)), function(i) {
    tibble(
      raw_row_id = i,
      raw_string = raw_resp$input[i],
      alias      = alias_resp$input[idx[i, ] + 1],  # Python → R
      similarity = scores[i, ]
    )
  }))
}

# Import
col_alias <- readRDS(file.path(data_dir,"intermediate/col_alias.rds"))
unmatched_col <- readRDS(file.path(data_dir,"intermediate/unmatched_col.rds"))
hs_alias <- readRDS(file.path(data_dir,"intermediate/hs_alias.rds"))
unmatched_hs <- readRDS(file.path(data_dir,"intermediate/unmatched_hs.rds"))

col_alias_strings = unique(col_alias[, c("alias"), with = FALSE])
hs_alias_strings = unique(hs_alias[, c("alias"), with = FALSE])


# Convert the input strings into embeddings
col_raw_input = pull(unmatched_col, clean_name)
col_raw_resp <- batch_embed(
  strings = col_raw_input,
  model = "text-embedding-3-large",
  batch_size = 1000
)

# Convert aliases into emeddings
col_alias_input = pull(col_alias_strings, alias)
col_alias_resp <- batch_embed(
  strings = col_alias_input,
  model = "text-embedding-3-large",
  batch_size = 1000
)
# Add row numbers to simplify comparison
col_alias_embed <- col_alias_resp %>%
  mutate(row_id = row_number())

# Convert alias to matrix
col_alias_embed_matrix <- col_alias_embed %>%
  pull(embedding) %>%
  do.call(rbind, .)


# Identify the top embedding for each of the randomly selected college strings
col_faiss_index <- build_faiss_index(col_alias_embed_matrix)
col_raw_embed = top_embed(raw_resp = col_raw_resp,
                          alias_resp = col_alias_embed,
                          index = col_faiss_index,
                          n = 5)

col_ambig_flag <- col_raw_embed %>%
  as_data_frame() %>%
  filter(similarity > col_threshold) %>%
  left_join(col_alias, by = "alias") %>%
  group_by(raw_string) %>%
  summarize(ambiguous_name = as.integer(dplyr::n_distinct(interaction(unitid,opeid,drop=TRUE)) > 1),
            .groups = "drop")

col_embed <- col_raw_embed %>%
  as_data_frame() %>%
  group_by(raw_row_id) %>%
  slice_max(n=1,order_by=similarity) %>%
  filter(similarity > col_threshold) %>%
  left_join(col_alias %>% distinct(), by = "alias") %>%
  left_join(unmatched_col, by = c("raw_string" = "clean_name")) %>%
  left_join(col_ambig_flag, by = "raw_string") %>%
  mutate(method = "Embedding") %>%
  select(-raw_row_id) 
saveRDS(col_embed,file.path(data_dir,"intermediate/col_embed.rds"))

# Convert the input strings into embeddings
hs_raw_input = pull(unmatched_hs, clean_name)
hs_raw_resp <- batch_embed(
  strings = hs_raw_input,
  model = "text-embedding-3-large",
  batch_size = 1000
)

# Convert aliases into emeddings
hs_alias_input = pull(hs_alias_strings, alias)
hs_alias_resp <- batch_embed(
  strings = hs_alias_input,
  model = "text-embedding-3-large",
  batch_size = 1000
)
# Add row numbers to simplify comparison
hs_alias_embed <- hs_alias_resp %>%
  mutate(row_id = row_number())

# Convert alias to matrix
hs_alias_embed_matrix <- hs_alias_embed %>%
  pull(embedding) %>%
  do.call(rbind, .)


# Identify the top embedding for each of the randomly selected hslege strings
hs_faiss_index <- build_faiss_index(hs_alias_embed_matrix)
hs_raw_embed = top_embed(raw_resp = hs_raw_resp,
                          alias_resp = hs_alias_embed,
                          index = hs_faiss_index,
                          n = 5)

hs_ambig_flag <- hs_raw_embed %>%
  as_data_frame() %>%
  filter(similarity > hs_threshold) %>%
  left_join(hs_alias, by = "alias") %>%
  group_by(raw_string) %>%
  summarize(ambiguous_name = as.integer(dplyr::n_distinct(interaction(hs_id,drop=TRUE)) > 1),
            .groups = "drop")

hs_embed <- hs_raw_embed %>%
  as_data_frame() %>%
  group_by(raw_row_id) %>%
  slice_max(n=1,order_by=similarity) %>%
  filter(similarity > hs_threshold) %>%
  left_join(hs_alias %>% distinct(), by = "alias") %>%
  left_join(unmatched_hs, by = c("raw_string" = "clean_name")) %>%
  left_join(hs_ambig_flag, by = "raw_string") %>%
  mutate(method = "Embedding") %>%
  select(-raw_row_id) 
saveRDS(hs_embed,file.path(data_dir,"intermediate/hs_embed.rds"))

