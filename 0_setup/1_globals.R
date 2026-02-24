# Project root ----
here::i_am("0_setup/1_globals.R")
project_root <- here::here()

# Reproducibility ----
GLOBALS <- list(
  seed = 202504,
  seed2 = 202601
)

set.seed(GLOBALS$seed)
RNGkind(sample.kind = "Rejection") # ensures sampling reproducibility across R versions
options(dplyr.summarise.inform = FALSE)


# Global path shortcuts ----
PATHS <- list(
  raw = file.path(project_root, "data", "raw"),
  processed = file.path(project_root, "data", "processed"),
  cv = file.path(project_root, "data", "cv"),
  lab_sets = file.path(project_root, "data", "lab_sets"),
  outputs = file.path(project_root, "outputs"),
  figures = file.path(project_root, "outputs", "figures"),
  tables = file.path(project_root, "outputs", "tables"),
  models = file.path(project_root, "outputs", "models"),
  results = file.path(project_root, "outputs", "results")
)

for (p in PATHS) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
} # ensuring the directories exist

# Global options ----
options(
  scipen = 999,
  digits = 4,
  stringsAsFactors = FALSE,
  width = 100
)
