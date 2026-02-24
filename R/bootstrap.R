here::i_am("R/bootstrap.R")

source(here::here("R", "helpers.R"))
source(here::here("R", "path_utils.R"))
source(here::here("R", "input_checks.R"))
source(here::here("R", "config.R"))

CLI_OPTS <- parse_cli_options()
CONFIG <- get_sepsis_config(get_cli_option(CLI_OPTS, "config", here::here("config", "defaults.yml")))

PATHS <- list(
  raw = CONFIG$paths$data_raw,
  processed = CONFIG$paths$data_processed,
  cv = file.path(CONFIG$root, "data", "cv"),
  lab_sets = file.path(CONFIG$root, "data", "lab_sets"),
  outputs = file.path(CONFIG$root, "outputs"),
  figures = file.path(CONFIG$root, "outputs", "figures"),
  tables = file.path(CONFIG$root, "outputs", "tables"),
  models = CONFIG$paths$outputs_models,
  results = file.path(CONFIG$root, "outputs", "results"),
  data_reference = CONFIG$paths$data_reference,
  outputs_intermediate = CONFIG$paths$outputs_intermediate,
  outputs_models = CONFIG$paths$outputs_models,
  outputs_analysis = CONFIG$paths$outputs_analysis,
  outputs_evaluation = CONFIG$paths$outputs_evaluation,
  outputs_exports = CONFIG$paths$outputs_exports,
  ensemble_workspace = CONFIG$paths$ensemble_workspace,
  prevalence_dir = CONFIG$paths$prevalence_dir,
  eval_variant_truth = CONFIG$paths$eval_variant_truth,
  eval_variant_predicted = CONFIG$paths$eval_variant_predicted,
  eval_variant_comparison = CONFIG$paths$eval_variant_comparison
)

ensure_dirs(list(
  PATHS$raw, PATHS$processed, PATHS$data_reference,
  PATHS$outputs, PATHS$figures, PATHS$tables, PATHS$models, PATHS$results,
  PATHS$outputs_intermediate, PATHS$outputs_models, PATHS$outputs_analysis,
  PATHS$outputs_evaluation, PATHS$outputs_exports, PATHS$ensemble_workspace,
  PATHS$eval_variant_truth, PATHS$eval_variant_predicted, PATHS$eval_variant_comparison
))

source(here::here("R", "packages.R"))
if (file.exists(here::here("0_setup", "3_functions.R"))) {
  source(here::here("0_setup", "3_functions.R"))
}

GLOBALS <- list(seed = 202504, seed2 = 202601)
set.seed(GLOBALS$seed)
RNGkind(sample.kind = "Rejection")
options(
  dplyr.summarise.inform = FALSE,
  scipen = 999,
  digits = 4,
  stringsAsFactors = FALSE,
  width = 100
)
