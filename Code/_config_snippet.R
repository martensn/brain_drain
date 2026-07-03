# Canonical replacement for every `directory <- "..."` block in Code/,
# applied during Phase 5. Not sourced by anything -- reference only.

library(dotenv)
library(here)

load_dot_env(here::here(".env"))

directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg
data_dir  <- file.path(directory, "Data")
out_dir   <- file.path(directory, "Outputs")
