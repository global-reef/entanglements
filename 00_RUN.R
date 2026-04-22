### 00_RUN.R ####
library(tidyverse)

analysis_date <- format(Sys.Date(), "%Y_%m_%d")

# ensure working dir is project root
wd0 <- getwd()

### directories ####
data_raw_dir   <- "data_raw"
data_clean_dir <- "data_clean"
docs_dir       <- "docs"

dir.create(data_raw_dir,   showWarnings = FALSE)
dir.create(data_clean_dir, showWarnings = FALSE)
dir.create(docs_dir,       showWarnings = FALSE)

### output structure (in root) ####
output_dir <- file.path(paste0("Analysis_", analysis_date))
dir.create(output_dir, showWarnings = FALSE)

fits_dir    <- file.path(output_dir, "fits")
plots_dir   <- file.path(output_dir, "plots")
stats_dir   <- file.path(output_dir, "stats")
summ_dir    <- file.path(output_dir, "summaries")
eda_dir     <- file.path(output_dir, "eda")
tables_dir  <- file.path(output_dir, "tables")

dir.create(fits_dir,   showWarnings = FALSE)
dir.create(plots_dir,  showWarnings = FALSE)
dir.create(stats_dir,  showWarnings = FALSE)
dir.create(summ_dir,   showWarnings = FALSE)
dir.create(eda_dir,    showWarnings = FALSE)
dir.create(tables_dir, showWarnings = FALSE)

### helpers ####
format_p <- function(p) {
  ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))
}

save_obj <- function(x, filename, dir = summ_dir) {
  dir.create(dir, showWarnings = FALSE)
  saveRDS(x, file.path(dir, filename))
  invisible(TRUE)
}

### run log ####
writeLines(
  c(
    paste0("analysis_date: ", analysis_date),
    paste0("wd: ", normalizePath(getwd(), winslash = "/")),
    paste0("output_dir: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE)),
    paste0("run_time: ", Sys.time())
  ),
  con = file.path(output_dir, "run_log.txt")
)

### run pipeline ####
# source("01_CLEAN.R")
# source("01.5_JOIN.R")
source("02_EXPLORE.R")
source("03_VALUE.R")

message("Run complete: ", analysis_date)