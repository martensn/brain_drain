# [CHANGED 2026-07-25 -- Phase 1] Hardcoded POSIX path replaced; this machine's
# actual Python install. CODEBASE_AUDIT.md flagged the original path as a hard
# portability blocker on Windows -- move to .env if this needs to travel
# across machines again.
Sys.setenv(RETICULATE_PYTHON = "C:/Program Files/Python313/python.exe")
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

use_python("C:/Program Files/Python313/python.exe", required = TRUE)
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




#ed_li1 = read_delim(file.path(data_dir,"raw/revelio/raw_educ_STATES_AK-MD.csv"))
#ed_li2 = read_delim(file.path(data_dir,"raw/revelio/raw_educ_STATES_MI-WY.csv"))

#ed_li = rbind(ed_li1,ed_li2) %>% select(-c(world_rank)) %>% as.data.table()
#rm(ed_li2,ed_li1)
#gc()
# Remove redundant intermediate objects to save memory
# Filter out associates degrees
#ed_li = ed_li[degree != "Associate"]
#ed_li = ed_li[university_country %in% c("United States","\\N")]
#comb = ed_li[,.(N = sum(N)), by = university_name]

#shrunken = comb[N > 5]
#fwrite(shrunken,file.path(data_dir,"intermediate/hs_col_classification.csv"))

# Convert all possible aliases to embeddings
hs_alias = fread(file.path(data_dir,"intermediate/hs_alias.csv"))
hs_alias[, hs_alias_id := .GRP, by = alias]
col_alias = fread(file.path(data_dir,"intermediate/col_alias.csv"))
col_alias[, col_alias_id := .GRP, by = alias]
cc_alias = fread(file.path(data_dir,"intermediate/cc_alias.csv"))
cc_alias[, col_alias_id := .GRP, by = alias]
hs_alias_strings = unique(hs_alias[, c("alias"), with = FALSE])
col_alias_strings = unique(col_alias[, c("alias"), with = FALSE])
cc_alias_strings = unique(cc_alias[, c("alias"), with = FALSE])

#shrunken = fread(file.path(data_dir,"intermediate/hs_col_classification.csv"))
#shrunken[,clean_name := normalize_high_school(university_name)]
# Avoid accidental matches on high school
input_col = readRDS(file.path(data_dir,"intermediate/input_col.rds"))

setDT(input_col)

# Construct crosswalk from rsid to four-year institutions
col_p0 <- input_col[
  university_country == "United States" &
    degree %chin% c("Associate", "", "Bachelor", "High School")
][
  !hs_alias,
  on = .(clean_name = alias)
][
  !cc_alias,
  on = .(clean_name = alias)
]
col_p1 = col_p0[
  col_alias,
  on = .(clean_name = alias),
  nomatch=0
][
  ,string_method := "Exact alias match"
  ]
# Flag duplicate names
col_exact_ambig <- col_p1[
  ,
  .(
    ambiguous_name = as.integer(
      uniqueN(interaction(unitid, opeid, drop = TRUE)) > 1
    )
  ),
  by = clean_name
]
# Merge ambiguous flag into dataset
col_p2 <- col_p1[
  col_exact_ambig,
  on = "clean_name",
  ambiguous_name := i.ambiguous_name
]


# Construct crosswalk from rsid to two-year institutions
cc_p0 <- input_col[
  university_country == "United States" &
    degree %chin% c("Associate", "", "Bachelor", "High School")
][
  !hs_alias,
  on = .(clean_name = alias)
][
  !col_alias,
  on = .(clean_name = alias)
]
cc_p1 = cc_p0[
  cc_alias,
  on = .(clean_name = alias),
  nomatch=0
][
  ,string_method := "Exact alias match"
]
# Flag duplicate names
cc_exact_ambig <- cc_p1[
  ,
  .(
    ambiguous_name = as.integer(
      uniqueN(interaction(unitid, opeid, drop = TRUE)) > 1
    )
  ),
  by = clean_name
]
# Merge ambiguous flag into dataset
cc_p2 <- cc_p1[
  cc_exact_ambig,
  on = "clean_name",
  ambiguous_name := i.ambiguous_name
]


# Construct crosswalk from rsid to high school
hs_p0 <- input_col[
  university_country == "United States" &
    degree %chin% c("High School", "", "Bachelor", "Associate")
][
  !col_alias,
  on = .(clean_name = alias)
][
  !cc_alias,
  on = .(clean_name = alias)
]
hs_p1 = hs_p0[
  hs_alias,
  on = .(clean_name = alias),
  nomatch=0
][
  ,string_method := "Exact alias match"
]
# Flag duplicate names
hs_exact_ambig <- hs_p1[
  ,
  .(
    ambiguous_name = as.integer(
      uniqueN(hs_id) > 1
    )
  ),
  by = clean_name
]
# Merge ambiguous flag into dataset
hs_p2 <- hs_p1[
  hs_exact_ambig,
  on = "clean_name",
  ambiguous_name := i.ambiguous_name
]

matched_rsid <- unique(c(col_p2$rsid, cc_p2$rsid, hs_p2$rsid))

# Construct set of all aliases lacking an exact match
unm <- rbindlist(
  list(
    col_p0[!rsid %in% matched_rsid],
    cc_p0[!rsid %in% matched_rsid],
    hs_p0[!rsid %in% matched_rsid]
  ),
  use.names = TRUE,
  fill = TRUE
)[
  ,
  .(N = sum(N, na.rm = TRUE)),
  by = .(clean_name, rsid, university_name, degree)
]
# Create rsid-level degree distributions, which might help infer the category
unm_wide <- unm[
  ,
  .(N = sum(N, na.rm = TRUE)),
  by = .(clean_name, rsid, university_name, degree)
][
  ,
  total := sum(N),
  by = rsid
][
  ,
  shr_deg := N / total
][
  ,
  degree := tolower(degree)
][
  ,
  .(clean_name, rsid, university_name, total, degree, shr_deg)
] |>
  dcast(
    clean_name + rsid + university_name + total ~ degree,
    value.var = "shr_deg"
  )

unm_cc = unm_wide %>% filter(associate >= 0.50) %>% mutate(ed_type = "cc")
unm_col = unm_wide %>% filter(bachelor >= 0.50) %>% mutate(ed_type = "college")
unm_hs = unm_wide %>% filter(`high school` >= 0.50) %>% mutate(ed_type = "high_school")
unm_remaining = unm %>% 
  group_by(rsid,clean_name,university_name) %>% 
  summarize(N = sum(N)) %>% 
  filter(!rsid %in% unm_cc$rsid, !rsid %in% unm_col$rsid, !rsid %in% unm_hs$rsid)
unm_remaining[, ed_type := vapply(clean_name, classify_ed_string, character(1))]
unmatched_col = rbind(unm_remaining[ed_type == "college",.(clean_name,rsid,university_name,N)],
                      unm_col[,.(clean_name,rsid,university_name,N=total)])
unmatched_hs = rbind(unm_remaining[ed_type == "high_school",.(clean_name,rsid,university_name,N)],
                      unm_hs[,.(clean_name,rsid,university_name,N=total)])

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
  left_join(unmatched_col, by = c("raw_string" = "clean_name")) %>%
  left_join(col_ambig_flag, by = "raw_string") %>%
  mutate(string_method = "Embedding") %>%
  select(-raw_row_id) %>%
  select(university_name,clean_name=alias,N,inst_name,col_alias_id,rsid,unitid,opeid,system_indicator,ambiguous_name,parent_institution,string_method)

# Collapse exact matches so that each row is a distinct unitid-opeid-rsid pair
col_exact = col_p2 %>% 
  group_by(university_name,clean_name,inst_name,col_alias_id,rsid,unitid,opeid,system_indicator,ambiguous_name,parent_institution,string_method) %>%
  summarize(N = sum(N))

col_ = rbind(col_embed,col_exact)
remaining_col = unmatched_col[!rsid %in% col_$rsid]

saveRDS(col_,file.path(data_dir,"intermediate/col_strings.rds"))
fwrite(col_,file.path(data_dir,"intermediate/col_strings.csv"))
saveRDS(remaining_col,file.path(data_dir,"intermediate/unmatched_col.rds"))
fwrite(remaining_col,file.path(data_dir,"intermediate/unmatched_col.csv"))

# Try matching on community college names
unmatched_cc <- unique(
  rbindlist(
    list(
      unmatched_col[!rsid %in% col_$rsid, .(clean_name, rsid, university_name, N)],
      unm_cc[, .(clean_name, rsid, university_name, N = total)]
    ),
    use.names = TRUE
  ),
  by = "rsid"
)

# Convert the input strings into embeddings
cc_raw_input = pull(unmatched_cc, clean_name)
cc_raw_resp <- batch_embed(
  strings = cc_raw_input,
  model = "text-embedding-3-large",
  batch_size = 1000
)

# Convert aliases into emeddings
cc_alias_input = pull(cc_alias_strings, alias)
cc_alias_resp <- batch_embed(
  strings = cc_alias_input,
  model = "text-embedding-3-large",
  batch_size = 1000
)
# Add row numbers to simplify comparison
cc_alias_embed <- cc_alias_resp %>%
  mutate(row_id = row_number())

# Convert alias to matrix
cc_alias_embed_matrix <- cc_alias_embed %>%
  pull(embedding) %>%
  do.call(rbind, .)

# Identify the top embedding for each of the randomly selected college strings
cc_faiss_index <- build_faiss_index(cc_alias_embed_matrix)
cc_raw_embed = top_embed(raw_resp = cc_raw_resp,
                          alias_resp = cc_alias_embed,
                          index = cc_faiss_index,
                          n = 1)

cc_ambig_flag <- cc_raw_embed %>%
  as_data_frame() %>%
  filter(similarity > col_threshold) %>%
  left_join(cc_alias, by = "alias") %>%
  group_by(raw_string) %>%
  summarize(ambiguous_name = as.integer(dplyr::n_distinct(interaction(unitid,opeid,drop=TRUE)) > 1),
            .groups = "drop")

cc_embed <- cc_raw_embed %>%
  as_data_frame() %>%
  filter(similarity > col_threshold) %>%
  left_join(cc_alias, by = "alias") %>%
  left_join(unmatched_cc, by = c("raw_string" = "clean_name")) %>%
  left_join(cc_ambig_flag, by = "raw_string") %>%
  mutate(string_method = "Embedding") %>%
  select(-raw_row_id) %>%
  select(university_name,clean_name=alias,N,inst_name,col_alias_id,rsid,unitid,opeid,system_indicator,ambiguous_name,parent_institution,string_method)

# Collapse exact matches so that each row is a distinct unitid-opeid-rsid pair
cc_exact = cc_p2 %>% 
  group_by(university_name,clean_name,inst_name,col_alias_id,rsid,unitid,opeid,system_indicator,ambiguous_name,parent_institution,string_method) %>%
  summarize(N = sum(N))

cc_ = rbind(cc_embed,cc_exact)
remaining_cc = unmatched_cc[!rsid %in% cc_$rsid]

saveRDS(cc_,file.path(data_dir,"intermediate/cc_strings.rds"))
fwrite(cc_,file.path(data_dir,"intermediate/cc_strings.csv"))
saveRDS(remaining_cc,file.path(data_dir,"intermediate/unmatched_cc.rds"))
fwrite(remaining_cc,file.path(data_dir,"intermediate/unmatched_cc.csv"))

# Remember to de-duplicate hs_p0, col_p0, and cc_p0 before running through embeddings
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

hs_ambig_flag <- hs_raw_embed %>%
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
  left_join(unmatched_hs, by = c("raw_string" = "clean_name")) %>%
  left_join(hs_ambig_flag, by = "raw_string") %>%
  mutate(string_method = "Embedding") %>%
  select(-raw_row_id) %>%
  select(university_name,clean_name=alias,N,hs_alias_id,rsid,hs_id,ambiguous_name,string_method)


# Collaprse exact match file so that each opeid-unitid-usid is a distinct row
hs_exact = hs_p2 %>% 
  group_by(university_name,clean_name,hs_alias_id,rsid,hs_id,ambiguous_name,string_method) %>%
  summarize(N = sum(N))

hs_ = rbind(hs_embed, hs_exact)
remaining_hs = unmatched_hs[!rsid %in% hs_$rsid]

saveRDS(hs_,file.path(data_dir,"intermediate/hs_strings.rds"))
fwrite(hs_,file.path(data_dir,"intermediate/hs_strings.csv"))
