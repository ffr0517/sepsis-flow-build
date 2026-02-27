# Setup ----
source(here::here("R", "bootstrap.R"))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(httr2)
  library(jsonlite)
  library(tibble)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# Simple CLI helpers ----
parse_cli_options <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- list()
  if (!length(args)) return(opts)

  for (arg in args) {
    if (!startsWith(arg, "--")) next
    eq <- regexpr("=", arg, fixed = TRUE)
    if (eq <= 0) next
    key <- substring(arg, 3, eq - 1)
    val <- substring(arg, eq + 1)
    if (nzchar(key)) opts[[key]] <- val
  }

  opts
}

get_option <- function(cli_opts, cli_key, env_key, default = NULL) {
  cli_val <- cli_opts[[cli_key]]
  if (!is.null(cli_val) && nzchar(str_trim(as.character(cli_val)))) return(cli_val)
  env_val <- Sys.getenv(env_key, unset = "")
  if (nzchar(str_trim(env_val))) return(env_val)
  default
}

as_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  key <- tolower(str_trim(as.character(x[[1]])))
  if (!nzchar(key)) return(default)
  key %in% c("1", "true", "t", "yes", "y", "on")
}

as01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) x <- as.character(x)
  x <- as.character(x)
  x <- str_trim(x)
  dplyr::case_when(
    x %in% c("1", "TRUE", "True", "true", "Y", "y", "Yes", "yes") ~ 1L,
    x %in% c("0", "FALSE", "False", "false", "N", "n", "No", "no") ~ 0L,
    TRUE ~ suppressWarnings(as.integer(x))
  )
}

choose_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (!length(hit)) return(NULL)
  hit[[1]]
}

assert_required_columns <- function(df, cols, context) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(context, " missing required columns: ", paste(missing, collapse = ", "))
  }
}

first_present_numeric <- function(df, candidates, n = nrow(df)) {
  if (is.null(df) || !nrow(df)) return(rep(NA_real_, n))
  for (nm in candidates) {
    if (nm %in% names(df)) return(suppressWarnings(as.numeric(df[[nm]])))
  }
  rep(NA_real_, n)
}

first_present_character <- function(df, candidates, n = nrow(df)) {
  if (is.null(df) || !nrow(df)) return(rep(NA_character_, n))
  for (nm in candidates) {
    if (nm %in% names(df)) return(as.character(df[[nm]]))
  }
  rep(NA_character_, n)
}

# Config ----
cli_opts <- parse_cli_options()

use_local_apis <- as_flag(get_option(cli_opts, "use_local_apis", "USE_LOCAL_APIS", "FALSE"))
orch_base_url <- get_option(cli_opts, "orch_base_url", "ORCH_BASE_URL", "http://localhost:8000")
check_local_orchestrator <- as_flag(get_option(
  cli_opts,
  "check_local_orchestrator",
  "LOCAL_ORCH_HEALTH_CHECK",
  if (use_local_apis) "TRUE" else "FALSE"
))
local_health_max_checks <- suppressWarnings(as.integer(get_option(cli_opts, "local_health_max_checks", "LOCAL_HEALTH_MAX_CHECKS", "60")))
if (is.na(local_health_max_checks) || local_health_max_checks < 1) local_health_max_checks <- 60
local_health_poll_seconds <- suppressWarnings(as.numeric(get_option(cli_opts, "local_health_poll_seconds", "LOCAL_HEALTH_POLL_SECONDS", "2")))
if (!is.finite(local_health_poll_seconds) || local_health_poll_seconds <= 0) local_health_poll_seconds <- 2
local_health_timeout_seconds <- suppressWarnings(as.numeric(get_option(cli_opts, "local_health_timeout_seconds", "LOCAL_HEALTH_TIMEOUT_SECONDS", "3")))
if (!is.finite(local_health_timeout_seconds) || local_health_timeout_seconds <= 0) local_health_timeout_seconds <- 3
warmup_local_apis <- as_flag(get_option(cli_opts, "warmup_local_apis", "LOCAL_API_WARMUP", if (use_local_apis) "TRUE" else "FALSE"))

d1_default_base <- if (use_local_apis) "http://localhost:8001" else "https://sepsis-flow-d1-api.onrender.com"
d2_default_base <- if (use_local_apis) "http://localhost:8002" else "https://sepsis-flow-platform.onrender.com"

d1_api_base_url <- get_option(cli_opts, "d1_api_base_url", "D1_API_BASE_URL", d1_default_base)
d2_api_base_url <- get_option(cli_opts, "d2_api_base_url", "D2_API_BASE_URL", d2_default_base)

vote_threshold <- suppressWarnings(as.numeric(get_option(cli_opts, "vote_threshold", "API_VOTE_THRESHOLD", "0.5")))
if (!is.finite(vote_threshold)) vote_threshold <- 0.5

api_timeout_seconds <- suppressWarnings(as.numeric(get_option(cli_opts, "api_timeout_seconds", "API_TIMEOUT_SECONDS", "180")))
if (!is.finite(api_timeout_seconds) || api_timeout_seconds <= 0) api_timeout_seconds <- 180

api_retry_attempts <- suppressWarnings(as.integer(get_option(cli_opts, "api_retry_attempts", "API_RETRY_ATTEMPTS", "5")))
if (is.na(api_retry_attempts) || api_retry_attempts < 1) api_retry_attempts <- 5

default_results_dir <- if (exists("PATHS") && !is.null(PATHS$results_evaluation)) PATHS$results_evaluation else "."
default_tables_dir <- if (exists("PATHS") && !is.null(PATHS$tables_evaluation)) PATHS$tables_evaluation else default_results_dir
default_output_path <- file.path(default_results_dir, "basic_patient_probability_votes_day2.rds")
output_path <- path.expand(get_option(cli_opts, "output_path", "BASIC_EVAL_OUTPUT_PATH", default_output_path))
default_day1_output_path <- file.path(default_results_dir, "basic_patient_probability_votes_day1.rds")
day1_output_path <- path.expand(get_option(cli_opts, "day1_output_path", "BASIC_EVAL_DAY1_OUTPUT_PATH", default_day1_output_path))
default_bundle_output_path <- file.path(default_results_dir, "basic_patient_probability_votes_both_days_bundle.rds")
bundle_output_path <- path.expand(get_option(cli_opts, "bundle_output_path", "BASIC_EVAL_BUNDLE_OUTPUT_PATH", default_bundle_output_path))

write_csv <- as_flag(get_option(cli_opts, "write_csv", "BASIC_EVAL_WRITE_CSV", "FALSE"))
csv_output_path <- path.expand(get_option(cli_opts, "csv_output_path", "BASIC_EVAL_CSV_OUTPUT_PATH", sub("\\.rds$", ".csv", output_path)))
default_day1_csv_output_path <- file.path(default_tables_dir, "basic_patient_probability_votes_day1.csv")
day1_csv_output_path <- path.expand(get_option(cli_opts, "day1_csv_output_path", "BASIC_EVAL_DAY1_CSV_OUTPUT_PATH", default_day1_csv_output_path))
default_combined_csv_output_path <- file.path(default_tables_dir, "basic_patient_probability_votes_both_days_combined.csv")
combined_csv_output_path <- path.expand(get_option(cli_opts, "combined_csv_output_path", "BASIC_EVAL_COMBINED_CSV_OUTPUT_PATH", default_combined_csv_output_path))

# Always store all treatment levels in one combined object.
if (!is.null(cli_opts[["treatment_level"]]) || nzchar(Sys.getenv("BASIC_EVAL_TREATMENT_LEVEL", unset = ""))) {
  message("Ignoring treatment-level filter; script now always saves all treatment levels (1:5) in one output object.")
}
treatment_levels_keep <- 1:5

levels_requested <- paste0("L", 1:5)

baseline_fields <- c(
  "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
  "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
)

day2_extra_fields <- sprintf("LEVEL%d_TREATMENTS_D1_SAFE_0", 1:5)
day2_required_fields <- c(baseline_fields, day2_extra_fields)

prior_variants <- tibble::tribble(
  ~variant_id,                  ~prior_adjustment, ~scenario_prior_label, ~use_country, ~use_inpatient,
  "country_x_inpatient_prior", "country_x_inpatient", "Country x inpatient", TRUE,      TRUE
)

# Data loading ----
derive_country_from_site <- function(site_values) {
  dplyr::case_when(
    grepl("^bd", site_values, ignore.case = TRUE) ~ "Bangladesh",
    grepl("^id", site_values, ignore.case = TRUE) ~ "Indonesia",
    grepl("^kh", site_values, ignore.case = TRUE) ~ "Cambodia",
    grepl("^la", site_values, ignore.case = TRUE) ~ "Laos",
    grepl("^vn", site_values, ignore.case = TRUE) ~ "Vietnam",
    TRUE ~ NA_character_
  )
}

load_test_data <- function(cli_opts) {
  test_path <- get_option(cli_opts, "test_path", "TEST_RDS_PATH", default = NULL)
  measures_path <- get_option(cli_opts, "measures_path", "MEASURES_CSV_PATH", default = NULL)

  if (is.null(test_path) && exists("PATHS") && !is.null(PATHS$processed)) {
    candidate <- file.path(PATHS$processed, "test.rds")
    if (file.exists(candidate)) test_path <- candidate
  }
  if (is.null(measures_path) && exists("PATHS") && !is.null(PATHS$processed)) {
    candidate <- file.path(PATHS$processed, "individual_measures.csv")
    if (file.exists(candidate)) measures_path <- candidate
  }

  if (is.null(test_path) || !nzchar(test_path) || !file.exists(test_path)) {
    stop("Could not find test dataset. Set --test_path=... or TEST_RDS_PATH (or PATHS$processed/test.rds).")
  }

  raw_test <- readRDS(test_path)

  if (!is.null(measures_path) && nzchar(measures_path) && file.exists(measures_path)) {
    measures_df <- read.csv(measures_path)
    if ("label" %in% names(raw_test) && "Label" %in% names(measures_df)) {
      raw_test <- raw_test %>%
        left_join(measures_df, by = c("label" = "Label"))
    }
  }

  country_cols <- names(raw_test)[tolower(names(raw_test)) == "country"]
  site_cols <- names(raw_test)[tolower(names(raw_test)) == "site"]

  country_values <- if (length(country_cols)) {
    as.character(raw_test[[country_cols[[1]]]])
  } else {
    rep(NA_character_, nrow(raw_test))
  }

  if (length(site_cols)) {
    derived_country <- derive_country_from_site(raw_test[[site_cols[[1]]]])
    missing_country <- is.na(country_values) | !nzchar(str_trim(country_values))
    country_values[missing_country] <- derived_country[missing_country]
  }

  raw_test$country <- country_values
  raw_test
}

# Variant input helpers ----
normalize_country <- function(x) {
  out <- str_trim(as.character(x))
  out[out == "" | tolower(out) %in% c("na", "null")] <- NA_character_
  out
}

normalize_inpatient_status <- function(x) {
  key <- gsub("[^a-z0-9]", "", tolower(str_trim(as.character(x))))
  dplyr::case_when(
    key %in% c("i", "inpatient", "in", "ip", "1", "true", "yes", "y") ~ "Inpatient",
    key %in% c("o", "outpatient", "out", "op", "0", "false", "no", "n") ~ "Outpatient",
    TRUE ~ NA_character_
  )
}

extract_country_vector <- function(df) {
  country_col <- choose_first_existing(df, c("country", "Country", "COUNTRY", "site_country", "country_name"))
  if (is.null(country_col)) return(rep(NA_character_, nrow(df)))
  normalize_country(df[[country_col]])
}

extract_inpatient_vector <- function(df) {
  status_col <- choose_first_existing(df, c("inpatient_status", "Inpatient_Status", "INPATIENT_STATUS"))
  if (!is.null(status_col)) return(normalize_inpatient_status(df[[status_col]]))

  ipdopd_col <- choose_first_existing(df, c("ipdopd", "IPDOPD"))
  if (!is.null(ipdopd_col)) return(normalize_inpatient_status(df[[ipdopd_col]]))

  rep(NA_character_, nrow(df))
}

extract_patient_id <- function(df) {
  id_col <- choose_first_existing(df, c("patient_id", "PatientID", "patientid", "id", "ID", "label", "Label"))
  if (is.null(id_col)) return(sprintf("row_%s", seq_len(nrow(df))))
  out <- as.character(df[[id_col]])
  out[is.na(out) | !nzchar(str_trim(out))] <- sprintf("row_%s", which(is.na(out) | !nzchar(str_trim(out))))
  out
}

build_variant_inputs <- function(raw_test, variant_row) {
  assert_required_columns(raw_test, baseline_fields, "test data")
  assert_required_columns(raw_test, day2_required_fields, "test data")

  country_vec <- extract_country_vector(raw_test)
  inpatient_vec <- extract_inpatient_vector(raw_test)

  if (isTRUE(variant_row$use_country) && all(is.na(country_vec))) {
    stop("Variant '", variant_row$variant_id, "' needs country values but none were found in test data.")
  }
  if (isTRUE(variant_row$use_inpatient) && all(is.na(inpatient_vec))) {
    stop("Variant '", variant_row$variant_id, "' needs inpatient status values but none were found in test data.")
  }

  day1_input <- raw_test %>% transmute(across(all_of(baseline_fields)))
  day2_input <- raw_test %>% transmute(across(all_of(day2_required_fields)))

  if (isTRUE(variant_row$use_country)) {
    day1_input$country <- country_vec
    day2_input$country <- country_vec
  }
  if (isTRUE(variant_row$use_inpatient)) {
    day1_input$inpatient_status <- inpatient_vec
    day2_input$inpatient_status <- inpatient_vec
  }

  list(
    day1_input = day1_input,
    day2_input = day2_input,
    country = country_vec,
    inpatient_status = inpatient_vec
  )
}

# API helpers ----
extract_api_error <- function(body) {
  if (is.list(body)) {
    if (is.list(body$error) && !is.null(body$error$message)) return(as.character(body$error$message))
    if (!is.null(body$error)) return(as.character(body$error))
    if (!is.null(body$message)) return(as.character(body$message))
  }
  as.character(body)
}

request_json_with_retry_method <- function(
    url,
    method = "POST",
    payload = NULL,
    timeout_sec = api_timeout_seconds,
    attempts = api_retry_attempts
) {
  last_err <- NULL
  method <- toupper(str_trim(method))

  for (attempt in seq_len(attempts)) {
    resp <- tryCatch(
      {
        req <- httr2::request(url) |>
          httr2::req_method(method) |>
          httr2::req_headers("Accept" = "application/json") |>
          httr2::req_timeout(timeout_sec)

        if (!is.null(payload)) {
          req <- req |>
            httr2::req_headers("Content-Type" = "application/json") |>
            httr2::req_body_json(payload, auto_unbox = TRUE, na = "string")
        }

        req |>
          httr2::req_perform()
      },
      error = function(e) e
    )

    if (inherits(resp, "error")) {
      last_err <- resp$message
    } else {
      status <- httr2::resp_status(resp)
      body_text <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
      parsed <- tryCatch(
        httr2::resp_body_json(resp, simplifyVector = TRUE),
        error = function(e) {
          if (!nzchar(body_text)) return(NULL)
          tryCatch(jsonlite::fromJSON(body_text, simplifyVector = TRUE), error = function(e2) NULL)
        }
      )

      if (status < 400) {
        if (!is.null(parsed)) return(parsed)
        stop("API returned non-JSON success response: ", substr(body_text, 1, 500))
      }

      err_raw <- parsed %||% body_text
      err_msg <- extract_api_error(err_raw)
      if (!nzchar(str_trim(err_msg))) err_msg <- paste0("HTTP ", status, " with empty error body")

      last_err <- paste0("HTTP ", status, ": ", err_msg)
      if (status < 500 && status != 429) stop(last_err)
    }

    if (attempt < attempts) Sys.sleep(min(2 * attempt, 8))
  }

  stop("API request failed after ", attempts, " attempts. Last error: ", last_err)
}

request_json_with_retry <- function(url, payload, timeout_sec = api_timeout_seconds, attempts = api_retry_attempts) {
  request_json_with_retry_method(
    url = url,
    method = "POST",
    payload = payload,
    timeout_sec = timeout_sec,
    attempts = attempts
  )
}

wait_for_local_orchestrator_and_warmup <- function(
    orch_base_url,
    max_checks = local_health_max_checks,
    poll_seconds = local_health_poll_seconds,
    health_timeout_seconds = local_health_timeout_seconds,
    do_warmup = warmup_local_apis
) {
  health_url <- paste0(orch_base_url, "/health")
  warmup_url <- paste0(orch_base_url, "/warmup")

  message("Waiting for local orchestrator: ", health_url)
  ok <- FALSE
  health_resp <- NULL

  for (i in seq_len(max_checks)) {
    message("Health check attempt ", i, "/", max_checks, " -> ", health_url)
    health_resp <- tryCatch(
      request_json_with_retry_method(
        url = health_url,
        method = "GET",
        payload = NULL,
        timeout_sec = health_timeout_seconds,
        attempts = 1L
      ),
      error = function(e) e
    )

    if (!inherits(health_resp, "error") && is.list(health_resp) && isTRUE(health_resp$ok)) {
      ok <- TRUE
      break
    }

    if (i < max_checks) Sys.sleep(poll_seconds)
  }

  if (!ok) {
    if (inherits(health_resp, "error")) {
      stop("Local orchestrator health check failed: ", conditionMessage(health_resp))
    }
    stop("Local orchestrator did not become healthy at ", health_url, " after ", max_checks, " checks.")
  }

  message("Local orchestrator healthy.")

  if (isTRUE(do_warmup)) {
    message("Warming local downstream APIs via orchestrator: ", warmup_url)
    warmup_resp <- request_json_with_retry_method(
      url = warmup_url,
      method = "POST",
      payload = NULL
    )
    if (is.list(warmup_resp) && !is.null(warmup_resp$ok) && !isTRUE(warmup_resp$ok)) {
      stop("Local warmup returned ok=FALSE")
    }
    message("Local warmup complete.")
  }

  invisible(TRUE)
}

normalize_prediction_frame <- function(parsed) {
  if (is.null(parsed)) stop("API returned empty response body.")
  if (is.data.frame(parsed)) return(as_tibble(parsed))
  if (is.list(parsed) && length(parsed) > 0 && is.list(parsed[[1]])) return(bind_rows(parsed))
  if (is.list(parsed) && !is.null(names(parsed))) return(as_tibble(parsed))
  stop("Unexpected prediction response format.")
}

build_prediction_details <- function(pred_long, day, n_patients, variant_id) {
  expected_rows <- n_patients * length(levels_requested)
  if (nrow(pred_long) != expected_rows) {
    stop(
      "Unexpected number of prediction rows for day ", day,
      ". Expected ", expected_rows,
      ", got ", nrow(pred_long), "."
    )
  }

  pred_long <- pred_long %>%
    mutate(
      patient_index = ((row_number() - 1) %% n_patients) + 1L,
      level_index = ((row_number() - 1) %/% n_patients) + 1L,
      outcome_key = sprintf("LEVEL%d_TREATMENTS_D%d_SAFE_0", level_index, day)
    )

  p_50_50 <- if ("p_50_50" %in% names(pred_long)) as.numeric(pred_long$p_50_50) else as.numeric(pred_long$mean_predicted_probability)
  t_50_50 <- if ("t_50_50" %in% names(pred_long)) as.numeric(pred_long$t_50_50) else rep(vote_threshold, nrow(pred_long))

  if (variant_id == "normal") {
    p_use <- p_50_50
    t_use <- t_50_50
  } else {
    if (!("p_adj" %in% names(pred_long) && "t_adj" %in% names(pred_long))) {
      stop("Variant '", variant_id, "' expected p_adj/t_adj in API response.")
    }
    p_use <- as.numeric(pred_long$p_adj)
    t_use <- as.numeric(pred_long$t_adj)

    missing_adj <- is.na(p_use) | is.na(t_use)
    if (any(missing_adj)) {
      warning(
        "Variant '", variant_id,
        "' had missing adjusted p/t for some rows; falling back to raw values for those rows."
      )
      p_use[missing_adj] <- p_50_50[missing_adj]
      t_use[missing_adj] <- t_50_50[missing_adj]
    }
  }

  votes_above_threshold <- first_present_numeric(
    pred_long,
    c("n_votes_gt", "votes_above_threshold", "n_votes_above_threshold", "n_votes"),
    n = nrow(pred_long)
  )
  vote_denominator <- first_present_numeric(
    pred_long,
    c("n_heads", "n_models", "ensemble_size", "num_heads", "n_estimators"),
    n = nrow(pred_long)
  )
  vote_frac <- first_present_numeric(
    pred_long,
    c("vote_frac_gt", "vote_fraction_gt", "vote_frac", "vote_fraction", "pct_votes_gt"),
    n = nrow(pred_long)
  )

  # Some APIs may return percentages on 0-100 scale.
  vote_frac <- ifelse(is.finite(vote_frac) & vote_frac > 1, vote_frac / 100, vote_frac)

  if (all(is.na(vote_denominator)) && any(!is.na(votes_above_threshold))) {
    vote_denominator <- rep(120, nrow(pred_long))
  }

  vote_frac_calc <- ifelse(
    !is.na(votes_above_threshold) & !is.na(vote_denominator) & vote_denominator > 0,
    votes_above_threshold / vote_denominator,
    NA_real_
  )
  vote_frac_used <- ifelse(!is.na(vote_frac_calc), vote_frac_calc, vote_frac)

  pred_binary <- ifelse(is.na(p_use) | is.na(t_use), NA_integer_, as.integer(p_use > t_use))

  pred_long %>%
    transmute(
      patient_index,
      level_index,
      prediction_day = day,
      treatment_outcome = outcome_key,
      avg_predicted_probability_raw = p_50_50,
      threshold_raw = t_50_50,
      avg_predicted_probability = p_use,
      threshold_used = t_use,
      predicted_binary = pred_binary,
      api_vote_threshold_requested = vote_threshold,
      votes_above_threshold = votes_above_threshold,
      vote_denominator = vote_denominator,
      vote_fraction_above_threshold = vote_frac_used
    )
}

call_day_api <- function(day, input_df, variant_id) {
  base_url <- if (day == 1) d1_api_base_url else d2_api_base_url
  endpoint <- if (day == 1) "/predict/day1" else "/predict/day2"
  url <- paste0(base_url, endpoint, "?format=long&vote_threshold=", vote_threshold)
  payload <- list(data = input_df, levels = levels_requested)

  parsed <- request_json_with_retry(url, payload)
  pred_long <- normalize_prediction_frame(parsed)
  build_prediction_details(pred_long, day = day, n_patients = nrow(input_df), variant_id = variant_id)
}

replace_day2_day1_treatments_with_predictions <- function(day2_input, pred_day1_long) {
  d1_cols <- sprintf("LEVEL%d_TREATMENTS_D1_SAFE_0", 1:5)

  pred_day1_for_day2 <- pred_day1_long %>%
    transmute(
      patient_index,
      input_col = sprintf("LEVEL%d_TREATMENTS_D1_SAFE_0", level_index),
      pred_val = predicted_binary
    ) %>%
    pivot_wider(names_from = input_col, values_from = pred_val) %>%
    arrange(patient_index)

  assert_required_columns(pred_day1_for_day2, c("patient_index", d1_cols), "Day 1 predictions reshaped for Day 2 input")

  if (nrow(pred_day1_for_day2) != nrow(day2_input)) {
    stop(
      "Day 1 prediction rows do not match Day 2 input rows. Expected ",
      nrow(day2_input), ", got ", nrow(pred_day1_for_day2), "."
    )
  }

  out <- day2_input
  for (col in d1_cols) out[[col]] <- pred_day1_for_day2[[col]]
  out
}

# Output builder ----
build_truth_long_for_day <- function(raw_test, day) {
  required <- sprintf("LEVEL%d_TREATMENTS_D%d_SAFE_0", 1:5, day)
  assert_required_columns(raw_test, required, "test data")

  tibble(patient_index = seq_len(nrow(raw_test))) %>%
    bind_cols(raw_test[, required, drop = FALSE]) %>%
    pivot_longer(
      cols = all_of(required),
      names_to = "treatment_outcome",
      values_to = "actual_true_outcome_raw"
    ) %>%
    mutate(
      level_index = as.integer(str_match(treatment_outcome, "^LEVEL([1-5])_")[, 2]),
      actual_true_outcome = as01(actual_true_outcome_raw)
    ) %>%
    select(patient_index, level_index, treatment_outcome, actual_true_outcome)
}

finalize_prediction_output <- function(
    pred_long,
    truth_long,
    patient_meta,
    variant_row,
    day1_input_source = NA_character_,
    day1_input_source_label = NA_character_,
    scenario_suffix = NA_character_
) {
  pred_long %>%
    left_join(truth_long, by = c("patient_index", "level_index", "treatment_outcome")) %>%
    left_join(patient_meta, by = "patient_index") %>%
    mutate(
      prior_variant_id = variant_row$variant_id,
      prior_adjustment = variant_row$prior_adjustment,
      prior_adjustment_label = variant_row$scenario_prior_label,
      day1_input_source = day1_input_source,
      day1_input_source_label = day1_input_source_label,
      scenario = ifelse(
        is.na(scenario_suffix),
        prior_adjustment,
        paste(prior_adjustment, scenario_suffix, sep = "__")
      )
    ) %>%
    filter(level_index %in% treatment_levels_keep) %>%
    arrange(patient_index, prediction_day, level_index) %>%
    select(
      patient_index,
      patient_id,
      site,
      country,
      inpatient_status,
      prediction_day,
      level_index,
      treatment_outcome,
      prior_variant_id,
      prior_adjustment,
      prior_adjustment_label,
      day1_input_source,
      day1_input_source_label,
      scenario,
      avg_predicted_probability,
      threshold_used,
      predicted_binary,
      votes_above_threshold,
      vote_denominator,
      vote_fraction_above_threshold,
      actual_true_outcome,
      avg_predicted_probability_raw,
      threshold_raw,
      api_vote_threshold_requested
    )
}

build_patient_metadata <- function(raw_test, country_vec, inpatient_vec) {
  tibble(
    patient_index = seq_len(nrow(raw_test)),
    patient_id = extract_patient_id(raw_test),
    site = first_present_character(raw_test, c("site", "Site", "SITE"), nrow(raw_test)),
    country = country_vec,
    inpatient_status = inpatient_vec
  )
}

build_variant_scenario_outputs <- function(raw_test, variant_row) {
  message("Running variant: ", variant_row$variant_id, " (", variant_row$scenario_prior_label, ")")

  inputs <- build_variant_inputs(raw_test, variant_row)
  patient_meta <- build_patient_metadata(raw_test, inputs$country, inputs$inpatient_status)
  truth_day1_long <- build_truth_long_for_day(raw_test, day = 1)
  truth_day2_long <- build_truth_long_for_day(raw_test, day = 2)

  # Day 1 call (fully adjusted) also feeds the Day 2 "predicted Day 1" pipeline.
  pred_day1_long <- call_day_api(day = 1, input_df = inputs$day1_input, variant_id = variant_row$variant_id)
  day1_table <- finalize_prediction_output(
    pred_long = pred_day1_long,
    truth_long = truth_day1_long,
    patient_meta = patient_meta,
    variant_row = variant_row,
    day1_input_source = "not_applicable_day1_prediction",
    day1_input_source_label = "Not applicable (Day 1 prediction)",
    scenario_suffix = "day1"
  )

  day2_pred_input <- replace_day2_day1_treatments_with_predictions(inputs$day2_input, pred_day1_long)
  pred_day2_pred_d1 <- call_day_api(day = 2, input_df = day2_pred_input, variant_id = variant_row$variant_id)

  day2_table <- finalize_prediction_output(
    pred_long = pred_day2_pred_d1,
    truth_long = truth_day2_long,
    patient_meta = patient_meta,
    variant_row = variant_row,
    day1_input_source = "predicted_day1",
    day1_input_source_label = "Predicted Day 1",
    scenario_suffix = "predicted_day1"
  )

  list(
    day1 = day1_table,
    day2 = day2_table
  )
}

build_all_outputs <- function(raw_test) {
  per_variant <- purrr::map(
    seq_len(nrow(prior_variants)),
    function(i) {
      variant_row <- as.list(prior_variants[i, ])
      build_variant_scenario_outputs(raw_test, variant_row)
    }
  )

  day1_df <- purrr::map_dfr(per_variant, "day1")
  day2_df <- purrr::map_dfr(per_variant, "day2")
  combined_df <- bind_rows(day1_df, day2_df) %>%
    arrange(patient_index, prediction_day, level_index)

  list(
    day1 = day1_df,
    day2 = day2_df,
    combined = combined_df
  )
}

# Run ----
if (isTRUE(use_local_apis)) {
  if (isTRUE(check_local_orchestrator)) {
    wait_for_local_orchestrator_and_warmup(
      orch_base_url = orch_base_url,
      max_checks = local_health_max_checks,
      poll_seconds = local_health_poll_seconds,
      health_timeout_seconds = local_health_timeout_seconds,
      do_warmup = warmup_local_apis
    )
  } else {
    message("Skipping local orchestrator health/warmup checks; using direct local APIs.")
  }
}

message("Day 1 API base: ", d1_api_base_url)
message("Day 2 API base: ", d2_api_base_url)

raw_test <- load_test_data(cli_opts)
result_bundle <- build_all_outputs(raw_test)
result_day1_df <- result_bundle$day1
result_day2_df <- result_bundle$day2
result_combined_df <- result_bundle$combined

assign("patient_probability_votes_day1", result_day1_df, envir = .GlobalEnv)
assign("patient_probability_votes_day2", result_day2_df, envir = .GlobalEnv)
assign("patient_probability_votes_both_days", result_bundle, envir = .GlobalEnv)

dir.create(dirname(day1_output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(bundle_output_path), recursive = TRUE, showWarnings = FALSE)

# Keep day2 path stable for downstream scripts (e.g., 05_voters_pct_eval.R).
saveRDS(result_day1_df, day1_output_path)
saveRDS(result_day2_df, output_path)
saveRDS(result_bundle, bundle_output_path)

if (isTRUE(write_csv)) {
  dir.create(dirname(day1_csv_output_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(csv_output_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(combined_csv_output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(result_day1_df, day1_csv_output_path, row.names = FALSE)
  write.csv(result_day2_df, csv_output_path, row.names = FALSE)
  write.csv(result_combined_df, combined_csv_output_path, row.names = FALSE)
}

message("Saved Day 1 output dataframe (all treatments, fully adjusted): ", day1_output_path)
message("Saved Day 2 output dataframe (predicted Day 1 pathway, all treatments, fully adjusted): ", output_path)
message("Saved bundle with day1/day2 tables: ", bundle_output_path)
if (isTRUE(write_csv)) {
  message("Saved Day 1 CSV: ", day1_csv_output_path)
  message("Saved Day 2 CSV: ", csv_output_path)
  message("Saved combined (Day 1 + Day 2) CSV: ", combined_csv_output_path)
}

message(
  "Day 1 rows: ", nrow(result_day1_df),
  " | Day 2 rows: ", nrow(result_day2_df),
  " | Patients: ", dplyr::n_distinct(result_combined_df$patient_index)
)
message("Expected rows per patient: Day 1 = 5, Day 2 = 5 (predicted Day 1 pathway), combined = 10, if no missing rows.")

print(utils::head(result_day1_df, 10))
print(utils::head(result_day2_df, 10))

invisible(result_bundle)
