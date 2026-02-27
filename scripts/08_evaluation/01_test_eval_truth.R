# Setup ----
source(here::here("R", "bootstrap.R"))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(httr2)
  library(jsonlite)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

as_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  key <- tolower(str_trim(as.character(x[[1]])))
  if (!nzchar(key)) return(default)
  key %in% c("1", "true", "t", "yes", "y", "on")
}

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

# Config ----
save_dir <- PATHS$results_evaluation
save_dir <- path.expand(save_dir)
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

variant_results_dir <- file.path(save_dir, "prior_variant_results_D1_TRUTH")
dir.create(variant_results_dir, recursive = TRUE, showWarnings = FALSE)

d1_api_base_url <- Sys.getenv("D1_API_BASE_URL", unset = "https://sepsis-flow-d1-api.onrender.com")
d2_api_base_url <- Sys.getenv("D2_API_BASE_URL", unset = "https://sepsis-flow-platform.onrender.com")

vote_threshold <- suppressWarnings(as.numeric(Sys.getenv("API_VOTE_THRESHOLD", unset = "0.5")))
if (!is.finite(vote_threshold)) vote_threshold <- 0.5

api_timeout_seconds <- suppressWarnings(as.numeric(Sys.getenv("API_TIMEOUT_SECONDS", unset = "180")))
if (!is.finite(api_timeout_seconds) || api_timeout_seconds <= 0) api_timeout_seconds <- 180

api_retry_attempts <- suppressWarnings(as.integer(Sys.getenv("API_RETRY_ATTEMPTS", unset = "5")))
if (is.na(api_retry_attempts) || api_retry_attempts < 1) api_retry_attempts <- 5

bootstrap_iterations <- suppressWarnings(as.integer(Sys.getenv("BOOTSTRAP_ITERATIONS", unset = "2000")))
if (is.na(bootstrap_iterations) || bootstrap_iterations < 200) bootstrap_iterations <- 2000

baseline_fields <- c(
  "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
  "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
)

day2_extra_fields <- c(
  "LEVEL1_TREATMENTS_D1_SAFE_0",
  "LEVEL2_TREATMENTS_D1_SAFE_0",
  "LEVEL3_TREATMENTS_D1_SAFE_0",
  "LEVEL4_TREATMENTS_D1_SAFE_0",
  "LEVEL5_TREATMENTS_D1_SAFE_0"
)

day2_required_fields <- c(baseline_fields, day2_extra_fields)
levels_requested <- paste0("L", 1:5)

prior_variants <- tibble::tribble(
  ~variant_id,                  ~variant_label,                    ~use_country, ~use_inpatient,
  "normal",                    "Normal (50/50)",                 FALSE,        FALSE,
  "whole_prior",               "Outcome Priors (Whole)",         FALSE,        FALSE,
  "country_prior",             "Country Priors",                 TRUE,         FALSE,
  "inpatient_status_prior",    "Inpatient Status Priors",        FALSE,        TRUE,
  "country_x_inpatient_prior", "Country x Inpatient Priors",     TRUE,         TRUE
)

variant_ids_all <- prior_variants$variant_id

variant_aliases <- c(
  normal = "normal",
  `50_50` = "normal",
  whole = "whole_prior",
  whole_prior = "whole_prior",
  outcome_priors_whole = "whole_prior",
  country = "country_prior",
  country_prior = "country_prior",
  inpatient = "inpatient_status_prior",
  inpatient_status = "inpatient_status_prior",
  inpatient_status_prior = "inpatient_status_prior",
  country_x_inpatient = "country_x_inpatient_prior",
  country_x_inpatient_prior = "country_x_inpatient_prior"
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

load_test_data <- function() {
  if (exists("PATHS") && !is.null(PATHS$processed)) {
    test_path <- file.path(PATHS$processed, "test.rds")
    measures_path <- file.path(PATHS$processed, "individual_measures.csv")
  } else {
    test_path <- Sys.getenv("TEST_RDS_PATH", unset = "")
    measures_path <- Sys.getenv("MEASURES_CSV_PATH", unset = "")
  }

  if (!nzchar(test_path) || !file.exists(test_path)) {
    stop("Could not find test dataset. Set PATHS$processed/test.rds or TEST_RDS_PATH.")
  }

  raw_test <- readRDS(test_path)

  if (nzchar(measures_path) && file.exists(measures_path)) {
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

# Helpers ----
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

fmt_pct_ci_exact <- function(n_pos, n_total) {
  if (is.na(n_total) || n_total <= 0) return("-")
  test <- binom.test(n_pos, n_total, conf.level = 0.95)
  est <- test$estimate * 100
  low <- test$conf.int[1] * 100
  high <- test$conf.int[2] * 100
  sprintf("%.1f (%.1f-%.1f)", est, low, high)
}

min_level_yes_vec <- function(x) {
  idx <- which(x == 1)
  if (!length(idx)) return(0L)
  as.integer(min(idx))
}

choose_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (!length(hit)) return(NULL)
  hit[[1]]
}

normalize_country <- function(x) {
  out <- str_trim(as.character(x))
  out[out == "" | tolower(out) %in% c("na", "null")] <- NA_character_
  out
}

normalize_inpatient_status <- function(x) {
  key <- gsub("[^a-z0-9]", "", tolower(str_trim(as.character(x))))
  out <- dplyr::case_when(
    key %in% c("i", "inpatient", "in", "ip", "1", "true", "yes", "y") ~ "Inpatient",
    key %in% c("o", "outpatient", "out", "op", "0", "false", "no", "n") ~ "Outpatient",
    TRUE ~ NA_character_
  )
  out
}

extract_country_vector <- function(df) {
  country_col <- choose_first_existing(df, c("country", "Country", "COUNTRY", "site_country", "country_name"))
  if (is.null(country_col)) return(rep(NA_character_, nrow(df)))
  normalize_country(df[[country_col]])
}

extract_inpatient_vector <- function(df) {
  status_col <- choose_first_existing(df, c("inpatient_status", "Inpatient_Status", "INPATIENT_STATUS"))
  if (!is.null(status_col)) {
    return(normalize_inpatient_status(df[[status_col]]))
  }

  ipdopd_col <- choose_first_existing(df, c("ipdopd", "IPDOPD"))
  if (!is.null(ipdopd_col)) {
    return(normalize_inpatient_status(df[[ipdopd_col]]))
  }

  rep(NA_character_, nrow(df))
}

extract_ipdopd_code <- function(df, inpatient_status) {
  ipdopd_col <- choose_first_existing(df, c("ipdopd", "IPDOPD"))
  if (!is.null(ipdopd_col)) {
    out <- toupper(str_trim(as.character(df[[ipdopd_col]])))
    out[!(out %in% c("I", "O"))] <- NA_character_
    return(out)
  }

  dplyr::case_when(
    inpatient_status == "Inpatient" ~ "I",
    inpatient_status == "Outpatient" ~ "O",
    TRUE ~ NA_character_
  )
}

assert_required_columns <- function(df, cols, context) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(context, " missing required columns: ", paste(missing, collapse = ", "))
  }
}

normalize_variant_token <- function(x) {
  key <- tolower(str_trim(as.character(x)))
  key <- gsub("[^a-z0-9_]+", "_", key)
  key <- gsub("_+", "_", key)
  key <- gsub("^_|_$", "", key)
  key
}

parse_variant_ids <- function(spec, allowed = variant_ids_all) {
  raw <- str_trim(as.character(spec %||% "all"))
  if (!nzchar(raw) || tolower(raw) == "all") return(allowed)

  parts <- unlist(strsplit(raw, ",", fixed = TRUE), use.names = FALSE)
  parts <- parts[nzchar(str_trim(parts))]
  if (!length(parts)) return(allowed)

  mapped <- vapply(parts, function(part) {
    token <- normalize_variant_token(part)
    resolved <- variant_aliases[[token]] %||% token
    resolved
  }, FUN.VALUE = character(1))

  unknown <- unique(mapped[!(mapped %in% allowed)])
  if (length(unknown)) {
    stop(
      "Unknown variant id(s): ", paste(unknown, collapse = ", "),
      ". Allowed: ", paste(allowed, collapse = ", ")
    )
  }

  unique(mapped)
}

list_saved_variant_ids <- function(allowed = variant_ids_all) {
  keep <- vapply(
    allowed,
    function(vid) file.exists(file.path(variant_results_dir, vid, "result_bundle.rds")),
    FUN.VALUE = logical(1)
  )
  allowed[keep]
}

get_variant_row <- function(variant_id) {
  idx <- match(variant_id, prior_variants$variant_id)
  if (is.na(idx)) stop("Unknown variant id: ", variant_id)
  as.list(prior_variants[idx, ])
}

# Prevalence helpers (for whole-prior local adjustment) ----
resolve_prevalence_path <- function() {
  env_path <- trimws(Sys.getenv("LOCAL_PREVALENCE_TABLE_PATH", unset = ""))
  if (nzchar(env_path) && file.exists(env_path)) return(env_path)

  candidates <- c(
    file.path(PATHS$data_reference, "prevalence_all_nested.rds"),
    "strata prevalence adjustment/prevalence_all_nested.rds",
    "./strata prevalence adjustment/prevalence_all_nested.rds"
  )

  found <- candidates[file.exists(candidates)]
  if (!length(found)) {
    stop("Could not find prevalence_all_nested.rds in data/reference or LOCAL_PREVALENCE_TABLE_PATH.")
  }

  found[[1]]
}

adjust_probability_for_prevalence <- function(p_50_50, prevalence) {
  p <- as.numeric(p_50_50)
  pi <- as.numeric(prevalence)
  out <- rep(NA_real_, length(p))

  finite <- is.finite(p) & is.finite(pi)
  if (!any(finite)) return(out)

  p <- pmin(pmax(p, 0), 1)
  pi <- pmin(pmax(pi, 0), 1)

  out[finite & pi <= 0] <- 0
  out[finite & pi >= 1] <- 1

  idx_mid <- finite & pi > 0 & pi < 1
  out[idx_mid & p <= 0] <- 0
  out[idx_mid & p >= 1] <- 1

  idx_calc <- idx_mid & p > 0 & p < 1
  if (any(idx_calc)) {
    odds <- p[idx_calc] / (1 - p[idx_calc])
    prior_odds <- pi[idx_calc] / (1 - pi[idx_calc])
    adjusted_odds <- odds * prior_odds
    out[idx_calc] <- adjusted_odds / (1 + adjusted_odds)
  }

  out
}

prevalence_path <- resolve_prevalence_path()
prevalence_nested <- readRDS(prevalence_path)

whole_prevalence_lookup <- purrr::map_dbl(
  prevalence_nested$whole,
  ~ as.numeric(.x$prevalence[[1]])
)

# API helpers ----
extract_api_error <- function(body) {
  if (is.list(body)) {
    if (is.list(body$error) && !is.null(body$error$message)) return(as.character(body$error$message))
    if (!is.null(body$error)) return(as.character(body$error))
    if (!is.null(body$message)) return(as.character(body$message))
  }
  as.character(body)
}

request_json_with_retry <- function(url, payload, timeout_sec = api_timeout_seconds, attempts = api_retry_attempts) {
  last_err <- NULL

  for (attempt in seq_len(attempts)) {
    resp <- tryCatch(
      {
        httr2::request(url) |>
          httr2::req_headers("Content-Type" = "application/json", "Accept" = "application/json") |>
          httr2::req_body_json(payload, auto_unbox = TRUE, na = "string") |>
          httr2::req_timeout(timeout_sec) |>
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
      if (!nzchar(str_trim(err_msg))) {
        err_msg <- paste0("HTTP ", status, " with empty error body")
      }

      last_err <- paste0("HTTP ", status, ": ", err_msg)

      if (status < 500 && status != 429) {
        stop(last_err)
      }
    }

    if (attempt < attempts) {
      Sys.sleep(min(2 * attempt, 8))
    }
  }

  stop("API request failed after ", attempts, " attempts. Last error: ", last_err)
}

normalize_prediction_frame <- function(parsed) {
  if (is.null(parsed)) {
    stop("API returned empty response body.")
  }

  if (is.data.frame(parsed)) {
    return(as_tibble(parsed))
  }

  if (is.list(parsed) && length(parsed) > 0 && is.list(parsed[[1]])) {
    return(bind_rows(parsed))
  }

  if (is.list(parsed) && !is.null(names(parsed))) {
    return(as_tibble(parsed))
  }

  stop("Unexpected prediction response format.")
}

build_decision_predictions <- function(pred_long, day, n_patients, variant_id) {
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

  p_50_50 <- if ("p_50_50" %in% names(pred_long)) {
    as.numeric(pred_long$p_50_50)
  } else {
    as.numeric(pred_long$mean_predicted_probability)
  }

  t_50_50 <- if ("t_50_50" %in% names(pred_long)) {
    as.numeric(pred_long$t_50_50)
  } else {
    rep(vote_threshold, nrow(pred_long))
  }

  if (variant_id == "normal") {
    p_use <- p_50_50
    t_use <- t_50_50
  } else if (variant_id == "whole_prior") {
    prevalence <- whole_prevalence_lookup[pred_long$outcome_key]
    p_use <- adjust_probability_for_prevalence(p_50_50, prevalence)
    t_use <- adjust_probability_for_prevalence(t_50_50, prevalence)
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
        "' has missing adjusted values for some rows; falling back to raw 50/50 values for those rows."
      )
      p_use[missing_adj] <- p_50_50[missing_adj]
      t_use[missing_adj] <- t_50_50[missing_adj]
    }
  }

  pred_binary <- ifelse(is.na(p_use) | is.na(t_use), NA_integer_, as.integer(p_use > t_use))

  pred_long %>%
    transmute(
      patient_index,
      level_index,
      pred_binary,
      probability_used = p_use,
      threshold_used = t_use
    )
}

call_day_api <- function(day, input_df, variant_id) {
  base_url <- if (day == 1) d1_api_base_url else d2_api_base_url
  endpoint <- if (day == 1) "/predict/day1" else "/predict/day2"

  url <- paste0(base_url, endpoint, "?format=long&vote_threshold=", vote_threshold)
  payload <- list(data = input_df, levels = levels_requested)

  parsed <- request_json_with_retry(url, payload)
  pred_long <- normalize_prediction_frame(parsed)

  build_decision_predictions(
    pred_long = pred_long,
    day = day,
    n_patients = nrow(input_df),
    variant_id = variant_id
  )
}

pred_long_to_wide <- function(pred_long, day) {
  pred_long %>%
    transmute(
      patient_index,
      pred_col = sprintf("LEVEL%d_D%d_PRED", level_index, day),
      pred_val = pred_binary
    ) %>%
    pivot_wider(names_from = pred_col, values_from = pred_val)
}

build_truth_df <- function(raw_test) {
  out <- tibble(patient_index = seq_len(nrow(raw_test)))

  for (day in 1:2) {
    for (level in 1:5) {
      src_col <- sprintf("LEVEL%d_TREATMENTS_D%d_SAFE_0", level, day)
      dst_col <- sprintf("LEVEL%d_D%d_TRUTH", level, day)
      if (!(src_col %in% names(raw_test))) {
        stop("Missing truth column in test data: ", src_col)
      }
      out[[dst_col]] <- as01(raw_test[[src_col]])
    }
  }

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

  day1_input <- raw_test %>%
    transmute(across(all_of(baseline_fields)))

  day2_input <- raw_test %>%
    transmute(across(all_of(day2_required_fields)))

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
    ipdopd = extract_ipdopd_code(raw_test, inpatient_vec)
  )
}

build_final_df_for_variant <- function(raw_test, variant_row) {
  inputs <- build_variant_inputs(raw_test, variant_row)

  pred_day1_long <- call_day_api(day = 1, input_df = inputs$day1_input, variant_id = variant_row$variant_id)
  pred_day2_long <- call_day_api(day = 2, input_df = inputs$day2_input, variant_id = variant_row$variant_id)

  pred_day1_wide <- pred_long_to_wide(pred_day1_long, day = 1)
  pred_day2_wide <- pred_long_to_wide(pred_day2_long, day = 2)

  truth_df <- build_truth_df(raw_test)

  final_df <- truth_df %>%
    left_join(pred_day1_wide, by = "patient_index") %>%
    left_join(pred_day2_wide, by = "patient_index") %>%
    mutate(ipdopd = inputs$ipdopd)

  required_pred_cols <- c(
    sprintf("LEVEL%d_D1_PRED", 1:5),
    sprintf("LEVEL%d_D2_PRED", 1:5)
  )
  assert_required_columns(final_df, required_pred_cols, paste0("predictions for variant '", variant_row$variant_id, "'"))

  final_df
}

# Table builders ----
build_treatment_level_table <- function(final_df) {
  outcomes <- tidyr::expand_grid(Level = 1:5, Day = 1:2) %>%
    mutate(
      truth_col = sprintf("LEVEL%d_D%d_TRUTH", Level, Day),
      pred_col = sprintf("LEVEL%d_D%d_PRED", Level, Day),
      Outcome = sprintf("Day %d Level %d", Day, Level)
    )

  purrr::pmap_dfr(outcomes, function(Level, Day, truth_col, pred_col, Outcome) {
    truth <- as01(final_df[[truth_col]])
    pred <- as01(final_df[[pred_col]])

    ok <- complete.cases(truth, pred)
    truth <- truth[ok]
    pred <- pred[ok]

    n <- length(truth)
    n_pred_yes <- sum(pred == 1, na.rm = TRUE)
    n_obs_yes <- sum(truth == 1, na.rm = TRUE)
    tp <- sum(pred == 1 & truth == 1, na.rm = TRUE)

    prevalence_num <- if (n > 0) n_obs_yes / n else NA_real_
    prevalence_str <- fmt_pct_ci_exact(n_obs_yes, n)

    sens_num <- if (n_obs_yes > 0) tp / n_obs_yes else NA_real_
    fnr_num <- if (is.finite(sens_num)) 1 - sens_num else NA_real_

    sens_str <- fmt_pct_ci_exact(tp, n_obs_yes)
    ppv_str <- fmt_pct_ci_exact(tp, n_pred_yes)

    rer_str <- "-"
    if (is.finite(prevalence_num) && prevalence_num > 0 && n_pred_yes > 0) {
      ppv_test <- binom.test(tp, n_pred_yes, conf.level = 0.95)
      rer_est <- as.numeric(ppv_test$estimate) / prevalence_num
      rer_low <- ppv_test$conf.int[1] / prevalence_num
      rer_high <- ppv_test$conf.int[2] / prevalence_num
      rer_str <- sprintf("%.1f (%.1f-%.1f)", rer_est, rer_low, rer_high)
    } else if (is.finite(prevalence_num) && prevalence_num > 0 && n_pred_yes == 0) {
      rer_str <- "0.0 (0.0-0.0)"
    }

    tibble(
      Outcome = Outcome,
      `Prevalence % (95% CI)` = prevalence_str,
      `Sensitivity % (95% CI)` = sens_str,
      `PPV % (95% CI)` = ppv_str,
      `FNR %` = ifelse(is.finite(fnr_num), round(100 * fnr_num, 1), NA_real_),
      `Risk Enrichment (95% CI)` = rer_str
    )
  }) %>%
    arrange(Outcome)
}

summarise_patient_block <- function(df, B = bootstrap_iterations) {
  df <- df %>% filter(!is.na(H1))
  n <- nrow(df)
  if (n == 0) return(NULL)

  day1_pct <- df$H1 / 5
  day2_pct <- df$H2 / 5
  perfect_d1 <- df$H1 == 5
  perfect_d2 <- df$H2 == 5
  mae_vals <- df$mae_cross_day
  signed_vals <- df$signed_err_cross

  net_bias_vals <- ifelse(signed_vals > 0, 1, ifelse(signed_vals < 0, -1, 0))

  boot_d1 <- numeric(B)
  boot_d2 <- numeric(B)
  boot_p1 <- numeric(B)
  boot_p2 <- numeric(B)
  boot_mae <- numeric(B)
  boot_signed <- numeric(B)
  boot_bias <- numeric(B)

  for (b in seq_len(B)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    boot_d1[b] <- mean(day1_pct[idx], na.rm = TRUE)
    boot_d2[b] <- mean(day2_pct[idx], na.rm = TRUE)
    boot_p1[b] <- mean(perfect_d1[idx], na.rm = TRUE)
    boot_p2[b] <- mean(perfect_d2[idx], na.rm = TRUE)
    boot_mae[b] <- mean(mae_vals[idx], na.rm = TRUE)
    boot_signed[b] <- mean(signed_vals[idx], na.rm = TRUE)
    boot_bias[b] <- mean(net_bias_vals[idx], na.rm = TRUE)
  }

  tibble(
    `Mean Day 1 % correct (95% CI)` = sprintf(
      "%.1f (%.1f-%.1f)",
      100 * mean(day1_pct),
      100 * quantile(boot_d1, 0.025, na.rm = TRUE),
      100 * quantile(boot_d1, 0.975, na.rm = TRUE)
    ),
    `Mean Day 2 % correct (95% CI)` = sprintf(
      "%.1f (%.1f-%.1f)",
      100 * mean(day2_pct),
      100 * quantile(boot_d2, 0.025, na.rm = TRUE),
      100 * quantile(boot_d2, 0.975, na.rm = TRUE)
    ),
    `% perfectly predicted Day 1 (95% CI)` = sprintf(
      "%.1f (%.1f-%.1f)",
      100 * mean(perfect_d1),
      100 * quantile(boot_p1, 0.025, na.rm = TRUE),
      100 * quantile(boot_p1, 0.975, na.rm = TRUE)
    ),
    `% perfectly predicted Day 2 (95% CI)` = sprintf(
      "%.1f (%.1f-%.1f)",
      100 * mean(perfect_d2),
      100 * quantile(boot_p2, 0.025, na.rm = TRUE),
      100 * quantile(boot_p2, 0.975, na.rm = TRUE)
    ),
    `Cross-day MAE (levels) (95% CI)` = sprintf(
      "%.2f (%.2f-%.2f)",
      mean(mae_vals),
      quantile(boot_mae, 0.025, na.rm = TRUE),
      quantile(boot_mae, 0.975, na.rm = TRUE)
    ),
    `Signed mean error (levels) (95% CI)` = sprintf(
      "%.2f (%.2f-%.2f)",
      mean(signed_vals),
      quantile(boot_signed, 0.025, na.rm = TRUE),
      quantile(boot_signed, 0.975, na.rm = TRUE)
    ),
    `Net triage bias % (95% CI)` = sprintf(
      "%.1f (%.1f-%.1f)",
      100 * mean(net_bias_vals),
      100 * quantile(boot_bias, 0.025, na.rm = TRUE),
      100 * quantile(boot_bias, 0.975, na.rm = TRUE)
    )
  )
}

build_patient_level_tables <- function(final_df) {
  truth_cols <- c(
    "LEVEL1_D1_TRUTH", "LEVEL2_D1_TRUTH", "LEVEL3_D1_TRUTH", "LEVEL4_D1_TRUTH", "LEVEL5_D1_TRUTH",
    "LEVEL1_D2_TRUTH", "LEVEL2_D2_TRUTH", "LEVEL3_D2_TRUTH", "LEVEL4_D2_TRUTH", "LEVEL5_D2_TRUTH"
  )

  pred_cols <- c(
    "LEVEL1_D1_PRED", "LEVEL2_D1_PRED", "LEVEL3_D1_PRED", "LEVEL4_D1_PRED", "LEVEL5_D1_PRED",
    "LEVEL1_D2_PRED", "LEVEL2_D2_PRED", "LEVEL3_D2_PRED", "LEVEL4_D2_PRED", "LEVEL5_D2_PRED"
  )

  weights <- c(5, 4, 3, 2, 1, 5, 4, 3, 2, 1)

  traj_df <- final_df %>%
    mutate(
      across(all_of(truth_cols), as01),
      across(all_of(pred_cols), as01),
      ipdopd = toupper(str_trim(as.character(ipdopd)))
    ) %>%
    rowwise() %>%
    mutate(
      ok = all(complete.cases(c_across(all_of(truth_cols)), c_across(all_of(pred_cols)))),
      FN = ifelse(ok, sum(c_across(all_of(truth_cols)) == 1 & c_across(all_of(pred_cols)) == 0), NA_integer_),
      FP = ifelse(ok, sum(c_across(all_of(truth_cols)) == 0 & c_across(all_of(pred_cols)) == 1), NA_integer_),
      true_max_d1 = ifelse(ok, min_level_yes_vec(c_across(all_of(truth_cols[1:5]))), NA_integer_),
      pred_max_d1 = ifelse(ok, min_level_yes_vec(c_across(all_of(pred_cols[1:5]))), NA_integer_),
      true_max_d2 = ifelse(ok, min_level_yes_vec(c_across(all_of(truth_cols[6:10]))), NA_integer_),
      pred_max_d2 = ifelse(ok, min_level_yes_vec(c_across(all_of(pred_cols[6:10]))), NA_integer_),
      over_levels_d1 = ifelse(ok, pmax(0L, true_max_d1 - pred_max_d1), NA_integer_),
      under_levels_d1 = ifelse(ok, pmax(0L, pred_max_d1 - true_max_d1), NA_integer_),
      over_levels_d2 = ifelse(ok, pmax(0L, true_max_d2 - pred_max_d2), NA_integer_),
      under_levels_d2 = ifelse(ok, pmax(0L, pred_max_d2 - true_max_d2), NA_integer_),
      over_levels = ifelse(ok, over_levels_d1 + over_levels_d2, NA_integer_),
      under_levels = ifelse(ok, under_levels_d1 + under_levels_d2, NA_integer_),
      H1 = ifelse(ok, sum(c_across(all_of(pred_cols[1:5])) == c_across(all_of(truth_cols[1:5]))), NA_integer_),
      H2 = ifelse(ok, sum(c_across(all_of(pred_cols[6:10])) == c_across(all_of(truth_cols[6:10]))), NA_integer_),
      H = ifelse(ok, H1 + H2, NA_integer_),
      P = ifelse(ok, sum(c_across(all_of(pred_cols)) == 1), NA_integer_),
      T = ifelse(ok, sum(c_across(all_of(truth_cols)) == 1), NA_integer_),
      I = ifelse(ok, sum(c_across(all_of(pred_cols)) == 1 & c_across(all_of(truth_cols)) == 1), NA_integer_),
      U = ifelse(ok, P + T - I, NA_integer_),
      W = ifelse(ok, sum(weights * (c_across(all_of(pred_cols)) != c_across(all_of(truth_cols)))), NA_real_),
      W_pct = ifelse(ok, 100 * W / sum(weights), NA_real_),
      abs_err_d1 = ifelse(ok, abs(pred_max_d1 - true_max_d1), NA_real_),
      abs_err_d2 = ifelse(ok, abs(pred_max_d2 - true_max_d2), NA_real_),
      mae_cross_day = ifelse(ok, (abs_err_d1 + abs_err_d2) / 2, NA_real_),
      signed_err_d1 = ifelse(ok, pred_max_d1 - true_max_d1, NA_real_),
      signed_err_d2 = ifelse(ok, pred_max_d2 - true_max_d2, NA_real_),
      signed_err_cross = ifelse(ok, (signed_err_d1 + signed_err_d2) / 2, NA_real_)
    ) %>%
    ungroup()

  overall_label <- sprintf("Overall (n = %d)", nrow(traj_df))
  inpatient_label <- sprintf("Inpatient (n = %d)", sum(traj_df$ipdopd == "I", na.rm = TRUE))
  outpatient_label <- sprintf("Outpatient (n = %d)", sum(traj_df$ipdopd == "O", na.rm = TRUE))

  traj_df <- traj_df %>%
    mutate(
      Cohort = case_when(
        ipdopd == "I" ~ inpatient_label,
        ipdopd == "O" ~ outpatient_label,
        TRUE ~ NA_character_
      )
    )

  table2_overall <- summarise_patient_block(traj_df) %>%
    mutate(Cohort = overall_label)

  table2_by_cohort <- traj_df %>%
    filter(!is.na(Cohort)) %>%
    group_by(Cohort) %>%
    group_modify(~ summarise_patient_block(.x)) %>%
    ungroup()

  cohort_levels <- c(overall_label, inpatient_label, outpatient_label)

  table2 <- bind_rows(table2_overall, table2_by_cohort) %>%
    mutate(Cohort = factor(Cohort, levels = cohort_levels)) %>%
    arrange(Cohort) %>%
    select(Cohort, everything())

  table2_lean <- table2 %>%
    select(
      Cohort,
      `Mean Day 1 % correct (95% CI)`,
      `Mean Day 2 % correct (95% CI)`,
      `% perfectly predicted Day 1 (95% CI)`,
      `% perfectly predicted Day 2 (95% CI)`,
      `Signed mean error (levels) (95% CI)`,
      `Net triage bias % (95% CI)`
    )

  list(full = table2, lean = table2_lean)
}

evaluate_single_variant <- function(raw_test, variant_id) {
  variant_row <- get_variant_row(variant_id)
  message("Running variant: ", variant_row$variant_id, " [", variant_row$variant_label, "]")

  final_df_variant <- build_final_df_for_variant(raw_test, variant_row)
  table1_variant <- build_treatment_level_table(final_df_variant)
  table2_variant <- build_patient_level_tables(final_df_variant)

  list(
    variant_id = variant_row$variant_id,
    variant_label = variant_row$variant_label,
    final_df = final_df_variant,
    table1 = table1_variant,
    table2 = table2_variant$full,
    table2_lean = table2_variant$lean
  )
}

save_variant_bundle <- function(variant_result) {
  variant_id <- variant_result$variant_id
  variant_dir <- file.path(variant_results_dir, variant_id)
  dir.create(variant_dir, recursive = TRUE, showWarnings = FALSE)

  bundle <- list(
    variant_id = variant_result$variant_id,
    variant_label = variant_result$variant_label,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    result = variant_result
  )

  saveRDS(bundle, file = file.path(variant_dir, "result_bundle.rds"))
  saveRDS(variant_result$final_df, file = file.path(variant_dir, "final_df.rds"))
  saveRDS(variant_result$table1, file = file.path(variant_dir, "treatment_level_results.rds"))
  saveRDS(variant_result$table2, file = file.path(variant_dir, "full_patient_level_results.rds"))
  saveRDS(variant_result$table2_lean, file = file.path(variant_dir, "patient_level_results.rds"))
}

load_saved_variant_bundle <- function(variant_id) {
  bundle_path <- file.path(variant_results_dir, variant_id, "result_bundle.rds")
  if (!file.exists(bundle_path)) return(NULL)

  obj <- readRDS(bundle_path)
  if (is.list(obj) && !is.null(obj$result)) return(obj$result)

  if (is.list(obj) && all(c("variant_id", "variant_label", "final_df", "table1", "table2", "table2_lean") %in% names(obj))) {
    return(obj)
  }

  stop("Saved bundle format not recognized: ", bundle_path)
}

load_saved_variant_results <- function(variant_ids, fail_if_missing = TRUE) {
  out <- list()
  missing <- character(0)

  for (variant_id in variant_ids) {
    result <- load_saved_variant_bundle(variant_id)
    if (is.null(result)) {
      missing <- c(missing, variant_id)
    } else {
      out[[variant_id]] <- result
    }
  }

  if (fail_if_missing && length(missing) > 0) {
    stop(
      "Missing saved variant bundles for: ", paste(unique(missing), collapse = ", "),
      ". Run those variant(s) first with mode=run."
    )
  }

  out
}

build_combined_outputs <- function(results_by_variant) {
  if (!length(results_by_variant)) {
    stop("No variant results supplied for combined outputs.")
  }

  table1_all <- purrr::map_dfr(
    results_by_variant,
    ~ .x$table1 %>% mutate(Variant = .x$variant_label)
  ) %>%
    relocate(Variant)

  table2_all <- purrr::map_dfr(
    results_by_variant,
    ~ .x$table2 %>% mutate(Variant = .x$variant_label)
  ) %>%
    relocate(Variant)

  table2_lean_all <- purrr::map_dfr(
    results_by_variant,
    ~ .x$table2_lean %>% mutate(Variant = .x$variant_label)
  ) %>%
    relocate(Variant)

  list(
    table1_all = table1_all,
    table2_all = table2_all,
    table2_lean_all = table2_lean_all
  )
}

save_combined_outputs <- function(results_by_variant) {
  combined <- build_combined_outputs(results_by_variant)

  saveRDS(results_by_variant, file = file.path(save_dir, "api_eval_results_by_prior_variant.rds"))
  saveRDS(combined$table1_all, file = file.path(save_dir, "treatment_level_results_all_prior_variants.rds"))
  saveRDS(combined$table2_all, file = file.path(save_dir, "full_patient_level_results_all_prior_variants.rds"))
  saveRDS(combined$table2_lean_all, file = file.path(save_dir, "patient_level_results_all_prior_variants.rds"))

  if ("normal" %in% names(results_by_variant)) {
    saveRDS(results_by_variant$normal$table1, file = file.path(save_dir, "treatment_level_results.rds"))
    saveRDS(results_by_variant$normal$table2, file = file.path(save_dir, "full_patient_level_results.rds"))
    saveRDS(results_by_variant$normal$table2_lean, file = file.path(save_dir, "patient_level_results.rds"))
  }

  invisible(combined)
}

# Orchestration ----
# Examples:
# Rscript test_eval.R --mode=run --variants=whole_prior
# Rscript test_eval.R --mode=run --variants=country_prior,inpatient_status_prior
# Rscript test_eval.R --mode=combine --variants=saved
# Rscript test_eval.R --mode=combine --variants=normal,whole_prior,country_prior,inpatient_status_prior,country_x_inpatient_prior

cli_opts <- parse_cli_options()
run_mode <- tolower(get_option(cli_opts, "mode", "EVAL_MODE", default = "run"))
if (!(run_mode %in% c("run", "combine"))) {
  stop("Invalid mode: ", run_mode, ". Allowed modes: run, combine.")
}

if (run_mode == "combine") {
  variant_spec <- get_option(cli_opts, "variants", "EVAL_VARIANTS", default = "saved")

  selected_variants <- if (tolower(str_trim(variant_spec)) == "saved") {
    ids <- list_saved_variant_ids()
    if (!length(ids)) {
      stop("No saved variant bundles found in: ", variant_results_dir)
    }
    ids
  } else {
    parse_variant_ids(variant_spec)
  }

  results_by_variant <- load_saved_variant_results(selected_variants, fail_if_missing = TRUE)
  save_combined_outputs(results_by_variant)

  message(
    "Combined outputs saved for variants: ",
    paste(names(results_by_variant), collapse = ", "),
    " -> ", save_dir
  )
} else {
  variant_spec <- get_option(cli_opts, "variants", "EVAL_VARIANTS", default = "all")
  selected_variants <- parse_variant_ids(variant_spec)
  combine_after_run <- as_flag(get_option(cli_opts, "combine_after_run", "EVAL_COMBINE_AFTER_RUN", default = "FALSE"))

  raw_test <- load_test_data()
  results_by_variant <- list()

  for (variant_id in selected_variants) {
    result <- evaluate_single_variant(raw_test, variant_id)
    save_variant_bundle(result)
    results_by_variant[[variant_id]] <- result
  }

  message(
    "Saved per-variant outputs for: ",
    paste(names(results_by_variant), collapse = ", "),
    " -> ", variant_results_dir
  )

  if (combine_after_run) {
    save_combined_outputs(results_by_variant)
    message("Combined outputs also saved -> ", save_dir)
  }
}
