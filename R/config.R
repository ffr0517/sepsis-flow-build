get_sepsis_config <- function(config_path = NULL) {
  default_path <- here::here("config", "defaults.yml")
  config_path <- config_path %||% default_path

  cfg <- list()
  if (file.exists(config_path)) {
    if (requireNamespace("yaml", quietly = TRUE)) {
      cfg <- yaml::read_yaml(config_path)
      if (is.null(cfg)) cfg <- list()
    } else {
      # Fallback parser for simple key: value config files.
      lines <- readLines(config_path, warn = FALSE)
      lines <- trimws(lines)
      lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
      kv <- strsplit(lines, ":", fixed = TRUE)
      for (pair in kv) {
        if (length(pair) < 2) next
        key <- trimws(pair[[1]])
        val <- trimws(paste(pair[-1], collapse = ":"))
        val <- sub("^['\"]", "", val)
        val <- sub("['\"]$", "", val)
        if (identical(tolower(val), "true")) val <- TRUE
        if (identical(tolower(val), "false")) val <- FALSE
        cfg[[key]] <- val
      }
    }
  }

  root <- here::here()
  data_root <- cfg$data_root %||% file.path(root, "data")
  outputs_root <- cfg$outputs_root %||% cfg$artifacts_root %||% file.path(root, "outputs")

  list(
    root = root,
    config_path = config_path,
    eval_mode = cfg$eval_mode %||% Sys.getenv("EVAL_MODE", unset = "api"),
    overwrite = isTRUE(cfg$overwrite),
    paths = list(
      data_raw = file.path(data_root, "raw"),
      data_processed = file.path(data_root, "processed"),
      data_reference = file.path(data_root, "reference"),
      outputs_root = outputs_root,
      outputs_intermediate = file.path(outputs_root, "intermediate"),
      outputs_models = file.path(outputs_root, "models"),
      outputs_analysis = file.path(outputs_root, "analysis"),
      outputs_evaluation = file.path(outputs_root, "evaluation"),
      outputs_exports = file.path(outputs_root, "exports"),
      ensemble_workspace = file.path(outputs_root, "models", "ensemble_workspace"),
      prevalence_dir = file.path(data_root, "reference"),
      eval_variant_truth = file.path(outputs_root, "evaluation", "prior_variant_results_D1_TRUTH"),
      eval_variant_predicted = file.path(outputs_root, "evaluation", "prior_variant_results_D1_PREDICTED"),
      eval_variant_comparison = file.path(outputs_root, "evaluation", "prior_variant_results_D1_COMPARISON")
    )
  )
}
