library(dotenv)
library(here)

cat("here() resolves to:", here::here(), "\n")

load_dot_env(here::here(".env"))

data_dir <- file.path(Sys.getenv("BRAIN_DRAIN_ROOT"), "Data")
out_dir  <- file.path(Sys.getenv("BRAIN_DRAIN_ROOT"), "Outputs")

cat("BRAIN_DRAIN_ROOT:", Sys.getenv("BRAIN_DRAIN_ROOT"), "\n")
cat("data_dir:", data_dir, "-- exists:", dir.exists(data_dir), "\n")
cat("out_dir:", out_dir, "-- exists:", dir.exists(out_dir), "\n")
cat("OPENAI_API_KEY loaded:", nchar(Sys.getenv("OPENAI_API_KEY")) > 0, "\n")
cat("CENSUS_KEY loaded:", nchar(Sys.getenv("CENSUS_KEY")) > 0, "\n")

ok <- dir.exists(data_dir) && dir.exists(out_dir) &&
  nchar(Sys.getenv("OPENAI_API_KEY")) > 0 && nchar(Sys.getenv("CENSUS_KEY")) > 0
cat(if (ok) "PASS\n" else "FAIL\n")
