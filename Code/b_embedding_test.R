Sys.setenv(RETICULATE_PYTHON = "/usr/local/bin/python3")
library(readxl)
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

use_python("/usr/local/bin/python3", required = TRUE)
py_config()

set.seed(49)
directory = "/nfs/turbo/lsa-areynoso"
directory = "/Volumes/lsa-areynoso"


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

classify_col_at_threshold <- function(df, threshold) {
  
  df %>%
    mutate(
      pred_is_match = pred_sim >= threshold,
      true_is_match = !is.na(true_unitid),
      
      outcome = case_when(
        pred_is_match & true_is_match & pred_unitid == true_unitid ~ "TP",
        pred_is_match & true_is_match & pred_unitid != true_unitid ~ "FP",
        pred_is_match & !true_is_match                            ~ "FP",
        !pred_is_match & true_is_match                            ~ "FN",
        !pred_is_match & !true_is_match                           ~ "TN"
      )
    )
}

classify_hs_at_threshold <- function(df, threshold) {
  df %>%
    mutate(
      pred_is_match = pred_sim >= threshold,
      outcome = case_when(
        pred_is_match & has_truth & is_correct     ~ "TP",
        pred_is_match & (!has_truth | !is_correct) ~ "FP",
        !pred_is_match & has_truth & is_correct    ~ "FN",
        !pred_is_match & (!has_truth | !is_correct)~ "TN"
      )
    )
}

hs_alias = fread(file.path(directory,"Data", "hs_alias.csv"))
hs_alias[, hs_alias_id := .GRP, by = alias]
col_alias = fread(file.path(directory,"Data","col_alias.csv"))
col_alias[, col_alias_id := .GRP, by = alias]
hs_alias_strings = unique(hs_alias[, c("alias"), with = FALSE])
col_alias_strings = unique(col_alias[, c("alias"), with = FALSE])
shrunken = fread(file.path(directory,"Data","hs_col_classification.csv"))
shrunken[,clean_name := normalize_high_school(university_name)]

# join + flag aliases which aren't particular to a single unitid
col_exact <- col_alias[
  shrunken[clean_name %in% col_alias$alias],
  on = .(alias = clean_name),
  nomatch = 0
][
  , ambiguous_name := as.integer(.N > 1),
  by = .(alias)
]
col_exact[,match_method := "Exact alias match"]

# join + flag aliases which aren't particular to a single hs_id
hs_exact <- hs_alias[
  shrunken[clean_name %in% hs_alias$alias & !clean_name %in% col_exact$alias],
  on = .(alias = clean_name),
  nomatch = 0
][
  , ambiguous_name := as.integer(.N > 1),
  by = .(university_name)
]
hs_exact[,match_method := "Exact alias match"]


# Create file of all string
unmatched = shrunken[!university_name %in% hs_exact$university_name & !university_name %in% col_exact$university_name]

# Begin by running on a random sample of strings
set.seed(5)
ss <- unmatched[sample(.N, 10000)]
ss[, ed_type := vapply(university_name, classify_ed_string, character(1))]
hs_ss = unique(ss[ed_type == "high_school", c("clean_name"), with = FALSE])
hs_raw_strings = hs_ss[sample(.N,500)]
col_ss = unique(ss[ed_type == "college", c("clean_name"), with = FALSE])
col_raw_strings = col_ss[sample(.N,500)]
# Save ground truth as .csv
hs_path  <- file.path(directory, "Data/hs_gt.csv")
col_path <- file.path(directory, "Data/col_gt.csv")

if (!file.exists(hs_path)) {
  write.csv(hs_raw_strings, hs_path, row.names = FALSE)
}

if (!file.exists(col_path)) {
  write.csv(col_raw_strings, col_path, row.names = FALSE)
}

# Convert the input strings into embeddings
#col_raw_input = pull(col_raw_strings, clean_name)
hs_raw_input = pull(hs_gt, clean_name)
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
saveRDS(hs_alias_resp,file.path(directory,"Data","hs_alias_resp.rds"))

# Add row numbers to simplify comparison
hs_alias_embed <- hs_alias_resp %>%
  mutate(row_id = row_number())

# Convert alias to matrix
hs_alias_embed_matrix <- hs_alias_embed %>%
  pull(embedding) %>%
  do.call(rbind, .)

# Identify the top five embeddings for each of the randomly selected hs strings
hs_faiss_index <- build_faiss_index(hs_alias_embed_matrix)
hs_embed = top_embed(raw_resp = hs_raw_resp, 
                     alias_resp = hs_alias_embed,
                     index = hs_faiss_index,
                     n=5)


# If the ground truth is unambiguous, then algorithm must match to a string 
# that's similarly unambiguous
hs_gtid = hs_gt %>%
  as_tibble() %>%
  select(-hs_id) %>%
  filter(!is.na(hs_alias)) %>%
  left_join(hs_alias %>% select(alias_weight,alias,hs_id), by = c("hs_alias"="alias")) 

# If only a single schools has the alias, embedding must find a closer match
hs_am = hs_gtid %>%
  group_by(hs_alias) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>%
  left_join(hs_alias %>% select(alias,hs_id), by = c("hs_alias"="alias")) %>%
  left_join(hs_gtid, by = c("hs_alias","hs_id")) %>%
  select(-c(hs_alias,n)) %>%
  inner_join(hs_alias %>% select(hs_id,alias), by = c("hs_id")) %>%
  group_by(alias,clean_name) %>%
  summarize() %>%
  mutate(ambiguously_named = 1,
         hs_id = NA) %>%
  rename(raw_string = clean_name)
# Identify all potential aliases from the nexus of ambiguously-named schools
hs_unam = hs_gtid %>% 
  group_by(hs_alias,clean_name) %>%
  summarize(n = n()) %>%
  filter(n == 1) %>%
  left_join(hs_alias %>% select(alias,hs_id), by = c("hs_alias"="alias")) %>%
  inner_join(hs_alias %>% select(hs_id,alias), by = c("hs_id")) %>%
  ungroup() %>%
  select(-c(n,hs_alias)) %>%
  mutate(ambiguously_named = 0) %>%
  rename(raw_string = clean_name)
hs_gt_aliases = rbind(hs_unam,hs_am)  


# Do the same for high schools
hs_preds <- hs_embed %>%
  group_by(raw_row_id, raw_string) %>%
  slice_max(similarity, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(pred_alias = alias, pred_sim = similarity) %>%
  left_join(hs_gt_aliases %>% rename(gt_alias = alias),
            by = c("raw_string"),
            relationship = "one-to-many")

correct = hs_preds %>%
  filter(gt_alias == pred_alias) %>%
  rename(true_id = hs_id) %>%
  mutate(pred_id = true_id)

errors = hs_preds %>%
  filter(gt_alias != pred_alias | is.na(gt_alias)) %>%
  filter(!raw_row_id %in% tp$raw_row_id) %>%
  select(-c(gt_alias,ambiguously_named,hs_id)) %>%
  distinct() %>%
  left_join(hs_alias %>% select(alias,hs_id) %>% rename(pred_id = hs_id), by = c("pred_alias"="alias"), relationship="many-to-many") %>%
  group_by(raw_row_id,raw_string,pred_alias,pred_sim) %>%
  summarize(
    pred_id = if (n_distinct(pred_id) == 1)
      first(pred_id)
    else
      NA) %>%
  mutate(ambiguously_named = if_else(is.na(pred_id),1,0)) %>%
  left_join(hs_gt %>% rename(gt_alias = hs_alias, true_id = hs_id), by = c("raw_string"="clean_name"))

hs_pred_eval = rbind(correct,errors) %>%
  mutate(
    # did we accept the prediction at this threshold?
    # (we’ll compute pred_is_match inside the threshold function)
    # define "correct" depending on ambiguity
    is_correct = case_when(
      ambiguously_named == 1 ~ !is.na(gt_alias) & (pred_alias == gt_alias),
      TRUE                   ~ !is.na(true_id)  & !is.na(pred_id) & (pred_id == true_id)
    ),
    # define whether there is any "truth target" at all
    # (if gt_alias is missing for ambiguous rows, or true_id missing for unambiguous)
    has_truth = case_when(
      ambiguously_named == 1 ~ !is.na(gt_alias),
      TRUE                   ~ !is.na(true_id)
    )
  )



hs_metrics <- map_dfr(
  threshold_grid,
  function(t) {
    classify_hs_at_threshold(hs_pred_eval, t) %>%
      count(outcome) %>%
      mutate(threshold = t)
  }
)

hs_metrics_wide <- hs_metrics %>%
  pivot_wider(
    names_from = outcome,
    values_from = n,
    values_fill = 0
  ) %>%
  group_by(threshold) %>%
  summarize(
    TN = sum(TN),
    TP = sum(TP),
    FP = sum(FP),
    FN = sum(FN),
    .groups = "drop"
  ) %>%
  mutate(
    precision = TP / (TP + FP),
    recall    = TP / (TP + FN),
    f1        = 2 * precision * recall / (precision + recall),
    coverage  = (TP + FP) / (TP + FP + TN + FN)
  )

# Convert the input strings into embeddings
col_raw_input = pull(col_gt, clean_name)
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
saveRDS(col_alias_resp,file.path(directory,"Data","col_alias_resp.rds"))

# Add row numbers to simplify comparison
col_alias_embed <- col_alias_resp %>%
  mutate(row_id = row_number())

# Convert alias to matrix
col_alias_embed_matrix <- col_alias_embed %>%
  pull(embedding) %>%
  do.call(rbind, .)


# Identify the top five embeddings for each of the randomly selected college strings
col_embed = top_embed(raw_resp = col_raw_resp, 
                      alias_resp = col_alias_embed,
                      alias_matrix = col_alias_embed_matrix,
                      n=5)

# Create df for comparing college embedding to ground truth
col_pred_eval <- col_embed %>%
  group_by(raw_row_id, raw_string) %>%
  slice_max(similarity, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(raw_row_id,
            raw_string,
            pred_alias = input,
            pred_sim   = similarity) %>%
  left_join(col_alias %>% select(alias, unitid, opeid) %>% distinct(),
            by = c("pred_alias" = "alias"),
            relationship = "many-to-many") %>%
  left_join(col_gt %>% rename(true_unitid = unitid, true_opeid  = opeid), 
            by = c("raw_string" = "clean_name")) %>%
  mutate(match = unitid == true_unitid) %>%
  group_by(raw_row_id, raw_string, pred_alias, pred_sim,true_unitid, true_opeid) %>%
  arrange(desc(match)) %>%   # prefer correct unit if present
  slice(1) %>%               # choose deterministically
  ungroup() %>%
  transmute(raw_row_id,
            raw_string,
            pred_alias,
            pred_sim,
            pred_unitid = unitid,
            pred_opeid  = opeid,
            true_unitid,
            true_opeid,
            matched_gt = match)

col_metrics <- map_dfr(
  threshold_grid,
  function(t) {
    classify_col_at_threshold(col_pred_eval, t) %>%
      count(outcome) %>%
      mutate(threshold = t)
  }
)

col_metrics_wide <- col_metrics %>%
  tidyr::pivot_wider(
    names_from = outcome,
    values_from = n,
    values_fill = 0
  ) %>%
  group_by(threshold)%>%
  summarize(TN = sum(TN),
            TP = sum(TP),
            FP = sum(FP),
            FN = sum(FN)) %>%
  mutate(
    precision = TP / (TP + FP),
    recall    = TP / (TP + FN),
    f1        = 2 * precision * recall / (precision + recall),
    coverage  = (TP + FP) / (TP + FP + TN + FN)
  )


