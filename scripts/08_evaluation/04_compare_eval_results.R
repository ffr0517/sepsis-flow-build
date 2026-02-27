# Compare D1_TRUTH vs D1_PREDICTED evaluation outputs by variant
# Produces 10 comparison table objects (5 variants x 2 table types),
# plus a country_x_inpatient patient-level lean comparison table.
source(here::here("R", "bootstrap.R"))

suppressPackageStartupMessages({
  library(dplyr)
})

save_dir <- PATHS$results_evaluation
save_dir <- path.expand(save_dir)

truth_results_dir <- file.path(save_dir, "prior_variant_results_D1_TRUTH")
pred_results_dir <- file.path(save_dir, "prior_variant_results_D1_PREDICTED")
comparison_results_dir <- file.path(save_dir, "prior_variant_results_D1_COMPARISON")
dir.create(comparison_results_dir, recursive = TRUE, showWarnings = FALSE)

variant_ids <- c(
  "normal",
  "whole_prior",
  "country_prior",
  "inpatient_status_prior",
  "country_x_inpatient_prior"
)

table_specs <- list(
  treatment_level = list(
    file = "treatment_level_results.rds",
    key_cols = "Outcome"
  ),
  patient_level = list(
    file = "patient_level_results.rds",
    key_cols = "Cohort"
  )
)

assert_exists <- function(path) {
  if (!file.exists(path)) stop("Missing file: ", path)
}

assert_unique_keys <- function(df, key_cols, context) {
  if (!all(key_cols %in% names(df))) {
    stop(context, " missing key column(s): ", paste(setdiff(key_cols, names(df)), collapse = ", "))
  }
  if (nrow(df) != nrow(unique(df[key_cols]))) {
    stop(context, " has duplicate key rows for: ", paste(key_cols, collapse = ", "))
  }
}

extract_point_estimate <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "-", "NA", "NaN", "NULL")] <- NA_character_

  has_num <- grepl("[-+]?[0-9]*\\.?[0-9]+", x_chr, perl = TRUE)
  num_chr <- rep(NA_character_, length(x_chr))
  num_chr[has_num] <- sub(
    ".*?([-+]?[0-9]*\\.?[0-9]+).*",
    "\\1",
    x_chr[has_num],
    perl = TRUE
  )
  suppressWarnings(as.numeric(num_chr))
}

round_change_col_1dp <- function(x) {
  x_chr <- as.character(x)
  keep_dash <- x_chr == "-"
  num <- suppressWarnings(as.numeric(x_chr))
  out <- x_chr
  out[!is.na(num)] <- sprintf("%.1f", round(num[!is.na(num)], 1))
  out[is.na(num) & !keep_dash] <- x_chr[is.na(num) & !keep_dash]
  out[keep_dash] <- "-"
  out
}

build_comparison_table <- function(truth_tbl, pred_tbl, key_cols, variant_id, table_name) {
  assert_unique_keys(truth_tbl, key_cols, paste0(variant_id, " / ", table_name, " (truth)"))
  assert_unique_keys(pred_tbl, key_cols, paste0(variant_id, " / ", table_name, " (pred)"))

  shared_cols <- intersect(names(truth_tbl), names(pred_tbl))
  metric_cols <- setdiff(shared_cols, key_cols)

  if (!length(metric_cols)) {
    stop("No shared metric columns found for ", variant_id, " / ", table_name)
  }

  truth_only <- setdiff(names(truth_tbl), names(pred_tbl))
  pred_only <- setdiff(names(pred_tbl), names(truth_tbl))
  if (length(truth_only)) {
    warning(variant_id, " / ", table_name, " truth-only columns ignored: ", paste(truth_only, collapse = ", "))
  }
  if (length(pred_only)) {
    warning(variant_id, " / ", table_name, " predicted-only columns ignored: ", paste(pred_only, collapse = ", "))
  }

  truth_renamed <- truth_tbl %>%
    rename_with(~ paste0(.x, "_D1_TRUTH"), all_of(metric_cols))

  pred_renamed <- pred_tbl %>%
    rename_with(~ paste0(.x, "_D1_PREDICTED"), all_of(metric_cols))

  out <- truth_renamed %>%
    full_join(pred_renamed, by = key_cols) %>%
    arrange(across(all_of(key_cols)))

  if (nrow(out) != nrow(truth_tbl) || nrow(out) != nrow(pred_tbl)) {
    stop("Row mismatch after join for ", variant_id, " / ", table_name)
  }

  for (metric in metric_cols) {
    truth_col <- paste0(metric, "_D1_TRUTH")
    pred_col <- paste0(metric, "_D1_PREDICTED")
    diff_col <- paste0(metric, "_DIFF_TRUTH_MINUS_PRED")

    out[[diff_col]] <- extract_point_estimate(out[[truth_col]]) - extract_point_estimate(out[[pred_col]])
  }

  out
}

build_country_x_inpatient_patient_level_lean <- function(comp_tbl) {
  required_cols <- c(
    "Cohort",
    "Mean Day 1 % correct (95% CI)_D1_TRUTH",
    "% perfectly predicted Day 1 (95% CI)_D1_TRUTH",
    "Mean Day 2 % correct (95% CI)_D1_PREDICTED",
    "% perfectly predicted Day 2 (95% CI)_D1_PREDICTED",
    "Signed mean error (levels) (95% CI)_D1_PREDICTED",
    "Net triage bias % (95% CI)_D1_PREDICTED",
    "Mean Day 2 % correct (95% CI)_DIFF_TRUTH_MINUS_PRED",
    "% perfectly predicted Day 2 (95% CI)_DIFF_TRUTH_MINUS_PRED",
    "Signed mean error (levels) (95% CI)_DIFF_TRUTH_MINUS_PRED",
    "Net triage bias % (95% CI)_DIFF_TRUTH_MINUS_PRED"
  )

  missing <- setdiff(required_cols, names(comp_tbl))
  if (length(missing)) {
    stop(
      "country_x_inpatient patient-level comparison table missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }

  out <- comp_tbl %>%
    select(all_of(required_cols)) %>%
    rename(
      `Day 1 mean % correct` = `Mean Day 1 % correct (95% CI)_D1_TRUTH`,
      `Day 1 % perfectly predicted` = `% perfectly predicted Day 1 (95% CI)_D1_TRUTH`,
      `Day 2 mean % correct (predicted Day 1)` = `Mean Day 2 % correct (95% CI)_D1_PREDICTED`,
      `Day 2 % perfectly predicted (predicted Day 1)` = `% perfectly predicted Day 2 (95% CI)_D1_PREDICTED`,
      `Signed mean error, levels (predicted Day 1)` = `Signed mean error (levels) (95% CI)_D1_PREDICTED`,
      `Net triage bias % (predicted Day 1)` = `Net triage bias % (95% CI)_D1_PREDICTED`,
      `Day 2 mean % correct: change if using observed Day 1` = `Mean Day 2 % correct (95% CI)_DIFF_TRUTH_MINUS_PRED`,
      `Day 2 % perfectly predicted: change if using observed Day 1` = `% perfectly predicted Day 2 (95% CI)_DIFF_TRUTH_MINUS_PRED`,
      `Signed mean error, levels: change if using observed Day 1` = `Signed mean error (levels) (95% CI)_DIFF_TRUTH_MINUS_PRED`,
      `Net triage bias %: change if using observed Day 1` = `Net triage bias % (95% CI)_DIFF_TRUTH_MINUS_PRED`
    )

  pct_change_cols <- c(
    "Day 2 mean % correct: change if using observed Day 1",
    "Day 2 % perfectly predicted: change if using observed Day 1",
    "Net triage bias %: change if using observed Day 1"
  )
  for (col in pct_change_cols) {
    out[[col]] <- round_change_col_1dp(out[[col]])
  }

  out
}

build_country_x_inpatient_treatment_level_lean <- function(comp_tbl) {
  required_cols <- c(
    "Outcome",
    "Prevalence % (95% CI)_D1_TRUTH",
    "Sensitivity % (95% CI)_D1_TRUTH",
    "PPV % (95% CI)_D1_TRUTH",
    "FNR %_D1_TRUTH",
    "Risk Enrichment (95% CI)_D1_TRUTH",
    "Prevalence % (95% CI)_D1_PREDICTED",
    "Sensitivity % (95% CI)_D1_PREDICTED",
    "PPV % (95% CI)_D1_PREDICTED",
    "FNR %_D1_PREDICTED",
    "Risk Enrichment (95% CI)_D1_PREDICTED",
    "Sensitivity % (95% CI)_DIFF_TRUTH_MINUS_PRED",
    "PPV % (95% CI)_DIFF_TRUTH_MINUS_PRED",
    "FNR %_DIFF_TRUTH_MINUS_PRED",
    "Risk Enrichment (95% CI)_DIFF_TRUTH_MINUS_PRED"
  )

  missing <- setdiff(required_cols, names(comp_tbl))
  if (length(missing)) {
    stop(
      "country_x_inpatient treatment-level comparison table missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }

  out <- comp_tbl %>%
    select(all_of(required_cols)) %>%
    rename(
      `Day 1 prevalence %` = `Prevalence % (95% CI)_D1_TRUTH`,
      `Day 1 sensitivity %` = `Sensitivity % (95% CI)_D1_TRUTH`,
      `Day 1 PPV %` = `PPV % (95% CI)_D1_TRUTH`,
      `Day 1 FNR %` = `FNR %_D1_TRUTH`,
      `Day 1 risk enrichment` = `Risk Enrichment (95% CI)_D1_TRUTH`,
      `Day 2 prevalence %` = `Prevalence % (95% CI)_D1_PREDICTED`,
      `Day 2 sensitivity % (predicted Day 1)` = `Sensitivity % (95% CI)_D1_PREDICTED`,
      `Day 2 PPV % (predicted Day 1)` = `PPV % (95% CI)_D1_PREDICTED`,
      `Day 2 FNR % (predicted Day 1)` = `FNR %_D1_PREDICTED`,
      `Day 2 risk enrichment (predicted Day 1)` = `Risk Enrichment (95% CI)_D1_PREDICTED`,
      `Day 2 sensitivity %: change if using observed Day 1` = `Sensitivity % (95% CI)_DIFF_TRUTH_MINUS_PRED`,
      `Day 2 PPV %: change if using observed Day 1` = `PPV % (95% CI)_DIFF_TRUTH_MINUS_PRED`,
      `Day 2 FNR %: change if using observed Day 1` = `FNR %_DIFF_TRUTH_MINUS_PRED`,
      `Day 2 risk enrichment: change if using observed Day 1` = `Risk Enrichment (95% CI)_DIFF_TRUTH_MINUS_PRED`
    )

  day1_rows <- grepl("^Day 1 Level [1-5]$", out$Outcome)
  day2_change_cols <- c(
    "Day 2 sensitivity %: change if using observed Day 1",
    "Day 2 PPV %: change if using observed Day 1",
    "Day 2 FNR %: change if using observed Day 1",
    "Day 2 risk enrichment: change if using observed Day 1"
  )
  for (col in day2_change_cols) {
    vals <- as.character(out[[col]])
    vals[day1_rows] <- "-"
    out[[col]] <- vals
  }

  pct_change_cols <- c(
    "Day 2 sensitivity %: change if using observed Day 1",
    "Day 2 PPV %: change if using observed Day 1",
    "Day 2 FNR %: change if using observed Day 1"
  )
  for (col in pct_change_cols) {
    out[[col]] <- round_change_col_1dp(out[[col]])
  }

  out
}

comparison_tables <- list()

for (variant_id in variant_ids) {
  for (table_name in names(table_specs)) {
    spec <- table_specs[[table_name]]

    truth_path <- file.path(truth_results_dir, variant_id, spec$file)
    pred_path <- file.path(pred_results_dir, variant_id, spec$file)

    assert_exists(truth_path)
    assert_exists(pred_path)

    truth_tbl <- readRDS(truth_path)
    pred_tbl <- readRDS(pred_path)

    comp_tbl <- build_comparison_table(
      truth_tbl = truth_tbl,
      pred_tbl = pred_tbl,
      key_cols = spec$key_cols,
      variant_id = variant_id,
      table_name = table_name
    )

    obj_name <- paste0(variant_id, "_", table_name, "_comparison")
    comparison_tables[[obj_name]] <- comp_tbl
    assign(obj_name, comp_tbl, envir = .GlobalEnv)

    saveRDS(comp_tbl, file = file.path(comparison_results_dir, paste0(obj_name, ".rds")))
  }
}

country_x_inpatient_patient_level_comparison_lean <- build_country_x_inpatient_patient_level_lean(
  comparison_tables[["country_x_inpatient_prior_patient_level_comparison"]]
)
assign(
  "country_x_inpatient_patient_level_comparison_lean",
  country_x_inpatient_patient_level_comparison_lean,
  envir = .GlobalEnv
)
saveRDS(
  country_x_inpatient_patient_level_comparison_lean,
  file = file.path(comparison_results_dir, "country_x_inpatient_patient_level_comparison_lean.rds")
)

country_x_inpatient_treatment_level_comparison_lean <- build_country_x_inpatient_treatment_level_lean(
  comparison_tables[["country_x_inpatient_prior_treatment_level_comparison"]]
)
assign(
  "country_x_inpatient_treatment_level_comparison_lean",
  country_x_inpatient_treatment_level_comparison_lean,
  envir = .GlobalEnv
)
saveRDS(
  country_x_inpatient_treatment_level_comparison_lean,
  file = file.path(comparison_results_dir, "country_x_inpatient_treatment_level_comparison_lean.rds")
)

saveRDS(
  comparison_tables,
  file = file.path(comparison_results_dir, "comparison_tables_all_variants.rds")
)

message("Created ", length(comparison_tables), " comparison tables in: ", comparison_results_dir)
message("Object names: ", paste(names(comparison_tables), collapse = ", "))
message("Created lean country_x_inpatient patient-level comparison table -> ", file.path(comparison_results_dir, "country_x_inpatient_patient_level_comparison_lean.rds"))
message("Created lean country_x_inpatient treatment-level comparison table -> ", file.path(comparison_results_dir, "country_x_inpatient_treatment_level_comparison_lean.rds"))
