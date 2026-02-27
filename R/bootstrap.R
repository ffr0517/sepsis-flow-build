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
  outputs = CONFIG$paths$outputs_root,
  figures = CONFIG$paths$outputs_figures,
  tables = CONFIG$paths$outputs_tables,
  models = CONFIG$paths$outputs_models,
  results = CONFIG$paths$outputs_results,
  data_reference = CONFIG$paths$data_reference,
  results_intermediate = CONFIG$paths$results_intermediate,
  results_analysis = CONFIG$paths$results_analysis,
  results_evaluation = CONFIG$paths$results_evaluation,
  results_exports = CONFIG$paths$results_exports,
  tables_preprocessing = CONFIG$paths$tables_preprocessing,
  tables_analysis = CONFIG$paths$tables_analysis,
  tables_evaluation = CONFIG$paths$tables_evaluation,
  figures_analysis = CONFIG$paths$figures_analysis,
  figures_evaluation = CONFIG$paths$figures_evaluation,
  ensemble_workspace = CONFIG$paths$ensemble_workspace,
  prevalence_dir = CONFIG$paths$prevalence_dir,
  eval_variant_truth = CONFIG$paths$eval_variant_truth,
  eval_variant_predicted = CONFIG$paths$eval_variant_predicted,
  eval_variant_comparison = CONFIG$paths$eval_variant_comparison
)

ensure_dirs(list(
  PATHS$raw, PATHS$processed, PATHS$data_reference,
  PATHS$outputs, PATHS$figures, PATHS$tables, PATHS$models, PATHS$results,
  PATHS$results_intermediate, PATHS$results_analysis, PATHS$results_evaluation, PATHS$results_exports,
  PATHS$tables_preprocessing, PATHS$tables_analysis, PATHS$tables_evaluation,
  PATHS$figures_analysis, PATHS$figures_evaluation, PATHS$ensemble_workspace,
  PATHS$eval_variant_truth, PATHS$eval_variant_predicted, PATHS$eval_variant_comparison
))

source(here::here("R", "packages.R"))

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
