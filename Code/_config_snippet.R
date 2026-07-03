# Canonical replacement for every `directory <- "..."` block in Code/,
# applied during Phase 5. Not sourced by anything -- reference only.

library(dotenv)
library(here)

load_dot_env(here::here(".env"))

data_dir <- file.path(Sys.getenv("BRAIN_DRAIN_ROOT"), "Data")
out_dir  <- file.path(Sys.getenv("BRAIN_DRAIN_ROOT"), "Outputs")
