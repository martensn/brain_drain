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
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
edufilename1 = "raw_educ_STATES_AK-MD.csv"
edufilename1edit = "STATES_AK-MD.csv"
edufilename2 = "raw_educ_STATES_MI-WY.csv"
edutfilename2edit = "STATES_MI-WY.csv"
misfitscorrection = "misfits_correction.csv"

# Minimum similarity score of top match 
hs_threshold = 0.91
col_threshold = 0.83

# normalize once
normalize_ed <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("\\s+", " ", trimws(x))
}

# strong signals
college_rx <- "\\b(university|univeristy|univ|u|college|colleges|institute of technology|polytechnic|(wharton|new) school|engineering|education|graduate|public health|management|mines|music|law|medicine|medical|business|pharmacy|economics|nursing|seminary)\\b"
hs_rx <- "\\b(high school|highschool|secondary|prep|preparatory|academy|school|high|h s|h. s.|h.s.|hs|schools|charter|senior|sr)\\b"

# negative evidence
non_school_rx <- "\\b(army|navy|air force|marines|company|corp|course|courses|coursework|society|class|seminar|services|consulting|training|sales|hospital|program|medical center|coach|coaching|test|tax|executive|acting|photography|bank|association|holistic|bartending|mixology|cosmetology|accounting|driving|tractor trailer|board|computer|extension|licensed|license|certificate|certification|certified|real estate|realtor|realtors|refrigeration|hair|nails|hypnotherapy|hypnotism|hypnotist|yoga|massage|professional|interior design|hairstyling|beauty|fire|police)\\b"

# weak signals (use only if undecided)
college_weak_rx <- "\\b(state|tech|poly|inst)\\b"
hs_weak_rx <- "\\b(district|isd|usd|county|regional|career center|technical center|career (and|&) technical|vocational|votech|vo-tech|catholic|collegiate|grammar)\\b"
foreign_rx <- paste0(
  "(?i)", # case-insensitive
  "(",
  # Romance languages
  "universit[aàáâãäåæ]|universidad|universidade|università|universitat|universitaire|universite|universitatea|universita|universiteti|uniwersytet|universitu00e0|accademia|nationale",
  "école|ecole|escuela|lycée|lycee|lyc|ecol|liceo|lyceum|colegio|colégio|istituto|instituto|seminario|scuola|catholique|escola|facultad|faculdades|faculdade|estudio|conservatorio|centro|comercio",
  # Germanic / Nordic
  "hochschule|fachhochschule|universität|gymnasium|gymnasiet|gimnasio|skole|skool|skola|seminarium|schule|collegium|Universitu00e4t|hogeschool",
  # Slavic / Eastern Europe
  "škola|skola|gimnazija|licej|akademija|universitet|universitetas|szkol",
  # Asian / transliterations
  "daxue|zhongxue|gaozhong|xueyuan|",
  # Generic
  "akademi|akademie|academia|tecnico|tecnológica|technologiko|tecnologico",
  "conservatoire|abroad|foreign|federazione",
  ")"
)

has_non_latin_script <- function(x) {
    # Detect CJK ranges explicitly (safe, fast)
    grepl(
      "[\u4E00-\u9FFF\u3040-\u30FF\uAC00-\uD7AF]",
      x
    )
  }

has_encoding_issue <- function(x) {
  # Any UTF-8 lead byte rendered as Latin-1 OR raw CJK blocks
  grepl(
    paste0(
      "(",
      "[ÃÄÅÆÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ]", "|",  # UTF-8 lead bytes
      "[â€˜â€™â€œâ€�â€“â€”]", "|",          # smart punctuation
      "[ä¸åæçèéêëìíîïñòóôõöøùúûüýþ]", "|", # common misdecoded bytes
      "[\\p{Han}\\p{Hiragana}\\p{Katakana}\\p{Hangul}]", # real CJK
      ")"
    ),
    x,
    perl = TRUE
  )
}

has_any_letter <- function(x) {
  grepl("\\p{L}", x, perl = TRUE)
}
is_not_school <- function(x) {
  # NA or empty or whitespace-only
  if (is.na(x) || grepl("^\\s*$", x)) return(TRUE)
  
  # No letters at all → only numbers / symbols / whitespace
  !has_any_letter(x)
}

has_mojibake_strong <- function(x) {
  grepl(
    paste0(
      "(",
      "Ã[\\x80-\\xBF]|",   # Ã© Ã§ Ã¯ Ã‰ etc
      "Å[\\x80-\\xBF]|",   # Å¡ Å¾ etc
      "Ä[\\x80-\\xBF]|",   # Äƒ etc
      "â€[\\x80-\\xBF]|",  # â€‹ â€™ â€“ etc
      "â„¢|â€œ|â€�|â€“|â€”",
      ")"
    ),
    x,
    perl = TRUE
  )
}


classify_ed_string <- function(x_raw) {
  
  x <- normalize_ed(x_raw)
  
  # ---- hard rejects ----
  if (is.na(x) || x == "") return("not_school")
  if (is_not_school(x))   return("not_school")
  if (grepl(non_school_rx, x)) return("not_school")
  
  # ---- foreign: non-latin scripts ----
  if (has_non_latin_script(x)) return("foreign")
  # ---- foreign: mojibake ----
  if (has_mojibake_strong(x)) return("foreign")
  # ---- foreign: education vocabulary ----
  if (grepl(foreign_rx, x)) return("foreign")
  
  # ---- college / HS (domestic only) ----
  if (grepl(college_rx, x)) return("college")
  if (grepl(hs_rx, x))      return("high_school")
  
  # ---- weak cues ----
  if (grepl(college_weak_rx, x)) return("college")
  if (grepl(hs_weak_rx, x))      return("high_school")
  
  "other"
}


normalize_high_school <- function(x) {
  # Work on lowercase copy
  x <- str_squish(tolower(x))
  
  # Replace variants with canonical "high school"
  x <- gsub(
    "\\b(h\\s*\\.?\\s*s\\.?|high\\s*school|highschool|high(?=\\s|$))\\b",
    "high school",
    x,
    perl = TRUE
  )
  x
}

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




ed_li1 = read_delim(file.path(data_dir,"raw/revelio/raw_educ_STATES_AK-MD.csv"))
ed_li2 = read_delim(file.path(data_dir,"raw/revelio/raw_educ_STATES_MI-WY.csv"))

ed_li = rbind(ed_li1,ed_li2) %>% select(-c(world_rank)) %>% as.data.table()
rm(ed_li2,ed_li1)
gc()
# Remove redundant intermediate objects to save memory
# Filter out associates degrees
#ed_li = ed_li[degree != "Associate"]
ed_li = ed_li[university_country %in% c("United States","\\N")]

# For data.table users (faster):
#k_hs[, ed_type := vapply(university_name, classify_ed_string, character(1))]
#k_col[, ed_type := vapply(university_name, classify_ed_string, character(1))]
#empties[, ed_type := vapply(university_name, classify_ed_string, character(1))]
#empties[,university_country := NULL]
#combined = rbind(k_hs,k_col,empties)
comb = ed_li[,.(N = sum(N)), by = university_name]

shrunken = comb[N > 5]
fwrite(shrunken,file.path(data_dir,"intermediate/hs_col_classification.csv"))

# Convert all possible aliases to embeddings
hs_alias = fread(file.path(data_dir,"intermediate/hs_alias.csv"))
hs_alias[, hs_alias_id := .GRP, by = alias]
col_alias = fread(file.path(data_dir,"intermediate/col_alias.csv"))
col_alias[, col_alias_id := .GRP, by = alias]
hs_alias_strings = unique(hs_alias[, c("alias"), with = FALSE])
col_alias_strings = unique(col_alias[, c("alias"), with = FALSE])
shrunken = fread(file.path(data_dir,"intermediate/hs_col_classification.csv"))
shrunken[,clean_name := normalize_high_school(university_name)]

# join + flag aliases which aren't particular to a single unitid
col_exact <- col_alias[
  shrunken[clean_name %in% col_alias$alias],
  on = .(alias = clean_name),
  nomatch = 0
][
  , ambiguous_name := as.integer(.N > 1),
  by = .(university_name)
]
col_exact[,raw_string := alias]
col_exact[,method := "Exact alias match"]
col_exact[,similarity := NA]


# join + flag aliases which aren't particular to a single hs_id
hs_exact <- hs_alias[
  shrunken[clean_name %in% hs_alias$alias & !clean_name %in% col_exact$alias],
  on = .(alias = clean_name),
  nomatch = 0
][
  , ambiguous_name := as.integer(.N > 1),
  by = .(university_name)
]
hs_exact[,raw_string := alias]
hs_exact[,method := "Exact alias match"]
hs_exact[,similarity := NA]

# Create file of all string
unmatched = shrunken[!university_name %in% hs_exact$university_name & !university_name %in% col_exact$university_name]
unmatched_hs = unmatched[ed_type == "high_school"]
unmatched_col = unmatched[ed_type == "college"]
unmatched_oth = unmatched[ed_type == "other"]
# Filter out obviously foreign words
unmatched_oth[, ed_type := vapply(university_name, classify_ed_string, character(1))]

unmatched_col = rbind(unmatched_oth[ed_type == "college"], unmatched_col)
unmatched_hs = rbind(unmatched_oth[ed_type == "high_school"], unmatched_hs)
# Begin by running on a random sample of strings
set.seed(5)
ss <- unmatched[sample(.N, 10000)]
ss[, ed_type := vapply(university_name, classify_ed_string, character(1))]
hs_ss = unique(ss[ed_type == "high_school", c("clean_name"), with = FALSE])
hs_raw_strings = hs_ss[sample(.N,500)]
col_ss = unique(ss[ed_type == "college", c("clean_name"), with = FALSE])
col_raw_strings = col_ss[sample(.N,500)]
# Save ground truth as .csv
hs_path  <- file.path(data_dir,"intermediate/hs_gt.csv")
col_path <- file.path(data_dir,"intermediate/col_gt.csv")

if (!file.exists(hs_path)) {
  write.csv(hs_raw_strings, hs_path, row.names = FALSE)
}

if (!file.exists(col_path)) {
  write.csv(col_raw_strings, col_path, row.names = FALSE)
}

# Convert the input strings into embeddings
#col_raw_input = pull(col_raw_strings, clean_name)
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

# Identify the top five embeddings for each of the randomly selected hs strings
hs_faiss_index <- build_faiss_index(hs_alias_embed_matrix)
hs_raw_embed = top_embed(raw_resp = hs_raw_resp,
                         alias_resp = hs_alias_embed,
                         index = hs_faiss_index,
                         n = 1)

ambig_flag <- hs_raw_embed %>%
  as_data_frame() %>%
  filter(similarity > hs_threshold) %>%
  left_join(hs_alias, by = "alias") %>%
  group_by(raw_string) %>%
  summarize(ambiguous_name = as.integer(dplyr::n_distinct(hs_id) > 1),
            .groups = "drop")

hs_embed <- hs_raw_embed %>%
  as_data_frame() %>%
  filter(similarity > hs_threshold) %>%
  left_join(hs_alias, by = "alias") %>%
  left_join(unmatched, by = c("raw_string" = "clean_name")) %>%
  left_join(ambig_flag, by = "raw_string") %>%
  mutate(method = "Embedding") %>%
  select(-raw_row_id) 

hs_ = rbind(hs_embed,hs_exact)
saveRDS(hs_,file.path(data_dir,"intermediate/hs_strings.rds"))
fwrite(hs_,file.path(data_dir,"intermediate/hs_strings.csv"))


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
                         n = 1)

col_ambig_flag <- col_raw_embed %>%
  as_data_frame() %>%
  filter(similarity > col_threshold) %>%
  left_join(col_alias, by = "alias") %>%
  group_by(raw_string) %>%
  summarize(ambiguous_name = as.integer(dplyr::n_distinct(interaction(unitid,opeid,drop=TRUE)) > 1),
            .groups = "drop")

col_embed <- col_raw_embed %>%
  as_data_frame() %>%
  filter(similarity > col_threshold) %>%
  left_join(col_alias, by = "alias") %>%
  left_join(unmatched, by = c("raw_string" = "clean_name")) %>%
  left_join(col_ambig_flag, by = "raw_string") %>%
  mutate(method = "Embedding") %>%
  select(-raw_row_id) 

col_ = rbind(col_embed,col_exact)
saveRDS(col_,file.path(data_dir,"intermediate/col_strings.rds"))
fwrite(col_,file.path(data_dir,"intermediate/col_strings.csv"))
