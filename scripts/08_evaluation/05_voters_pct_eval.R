# Setup ----
source(here::here("R", "bootstrap.R"))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(tibble)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

assert_required_columns <- function(df, cols, context) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(context, " missing required columns: ", paste(missing, collapse = ", "))
  }
}

fmt_ci_num <- function(est, low, high, digits = 2) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"), est, low, high)
}

# Paths ----
input_bundle_path <- file.path(PATHS$results_evaluation, "basic_patient_probability_votes_both_days_bundle.rds")
input_day1_rds_path <- file.path(PATHS$results_evaluation, "basic_patient_probability_votes_day1.rds")
input_day2_rds_path <- file.path(PATHS$results_evaluation, "basic_patient_probability_votes_day2.rds")

if (!file.exists(input_bundle_path) && !(file.exists(input_day1_rds_path) && file.exists(input_day2_rds_path))) {
  stop(
    "Could not find Day 1 + Day 2 inputs.\n",
    "Expected either bundle: ", input_bundle_path, "\n",
    "or both files: ", input_day1_rds_path, " and ", input_day2_rds_path, "\n",
    "Run scripts/08_evaluation/03_basic_patient_probability_votes_day2.R first."
  )
}

eval_out_dir <- file.path(PATHS$results_evaluation, "05_voters_pct_eval")
table_out_dir <- file.path(PATHS$tables_evaluation, "05_voters_pct_eval")
fig_out_dir <- file.path(PATHS$figures_evaluation, "05_voters_pct_eval")
dir.create(eval_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_out_dir, recursive = TRUE, showWarnings = FALSE)

# Labels ----
treatment_label_map <- c(
  "LEVEL1_TREATMENTS_D1_SAFE_0" = "Mechanical Ventilation",
  "LEVEL1_TREATMENTS_D2_SAFE_0" = "Mechanical Ventilation (Day 2)",
  "LEVEL2_TREATMENTS_D1_SAFE_0" = "CPAP or IV Fluid Bolus",
  "LEVEL2_TREATMENTS_D2_SAFE_0" = "CPAP or IV Fluid Bolus (Day 2)",
  "LEVEL3_TREATMENTS_D1_SAFE_0" = "ICU Admission",
  "LEVEL3_TREATMENTS_D2_SAFE_0" = "ICU Admission (Day 2)",
  "LEVEL4_TREATMENTS_D1_SAFE_0" = "Oxygen",
  "LEVEL4_TREATMENTS_D2_SAFE_0" = "Oxygen (Day 2)",
  "LEVEL5_TREATMENTS_D1_SAFE_0" = "Non-bolused IV Fluids",
  "LEVEL5_TREATMENTS_D2_SAFE_0" = "Non-bolused IV Fluids (Day 2)"
)

treatment_label_map[["LEVEL1_TREATMENTS_D2_SAFE_0"]] <- "Mechanical Ventilation"
treatment_label_map[["LEVEL2_TREATMENTS_D2_SAFE_0"]] <- "CPAP or IV Fluid Bolus"
treatment_label_map[["LEVEL3_TREATMENTS_D2_SAFE_0"]] <- "ICU Admission"
treatment_label_map[["LEVEL4_TREATMENTS_D2_SAFE_0"]] <- "Oxygen"
treatment_label_map[["LEVEL5_TREATMENTS_D2_SAFE_0"]] <- "Non-bolused IV Fluids"

treatment_order <- c(
  "Mechanical Ventilation",
  "CPAP or IV Fluid Bolus",
  "ICU Admission",
  "Oxygen",
  "Non-bolused IV Fluids"
)

prior_label_map <- c(
  "unadjusted" = "No Prior Adjustment",
  "country_x_inpatient" = "Country x Inpatient Prior"
)

day1_source_label_map <- c(
  "true_day1" = "True Day 1 Inputs",
  "predicted_day1" = "Predicted Day 1 Inputs"
)

# Load and clean ----
raw_input_obj <- if (file.exists(input_bundle_path)) readRDS(input_bundle_path) else NULL

raw_df <- if (is.list(raw_input_obj) && !is.null(raw_input_obj$combined)) {
  as_tibble(raw_input_obj$combined)
} else if (is.list(raw_input_obj) && !is.null(raw_input_obj$day1) && !is.null(raw_input_obj$day2)) {
  bind_rows(as_tibble(raw_input_obj$day1), as_tibble(raw_input_obj$day2))
} else if (file.exists(input_day1_rds_path) && file.exists(input_day2_rds_path)) {
  bind_rows(as_tibble(readRDS(input_day1_rds_path)), as_tibble(readRDS(input_day2_rds_path)))
} else if (!is.null(raw_input_obj) && is.data.frame(raw_input_obj)) {
  as_tibble(raw_input_obj)
} else {
  stop("Input object format not recognized for Day 1 + Day 2 voters_pct evaluation.")
}

assert_required_columns(
  raw_df,
  c(
    "patient_index", "patient_id", "prediction_day", "treatment_outcome",
    "prior_adjustment", "day1_input_source",
    "actual_true_outcome", "votes_above_threshold", "vote_denominator"
  ),
  "basic patient probability vote eval data"
)

d_all <- raw_df %>%
  mutate(
    patient_id = coalesce(as.character(patient_id), as.character(patient_index)),
    vote_fraction = suppressWarnings(as.numeric(votes_above_threshold)),
    vote_fraction = ifelse(is.finite(vote_fraction) & vote_fraction > 1, vote_fraction / 100, vote_fraction),
    vote_fraction = pmin(pmax(vote_fraction, 0), 1),
    vote_pct = 100 * vote_fraction,
    vote_denominator = suppressWarnings(as.numeric(vote_denominator)),
    vote_denominator = ifelse(!is.finite(vote_denominator) | vote_denominator <= 0, 120, vote_denominator),
    y = suppressWarnings(as.integer(actual_true_outcome)),
    prediction_day = suppressWarnings(as.integer(prediction_day)),
    prediction_day_label = dplyr::case_when(
      prediction_day == 1L ~ "Day 1",
      prediction_day == 2L ~ "Day 2",
      TRUE ~ NA_character_
    ),
    treatment_raw = as.character(treatment_outcome),
    treatment = dplyr::recode(treatment_raw, !!!treatment_label_map, .default = treatment_raw),
    prior_adjustment_raw = as.character(prior_adjustment),
    prior_adjustment_label = dplyr::recode(prior_adjustment_raw, !!!prior_label_map, .default = prior_adjustment_raw),
    day1_input_source_raw = as.character(day1_input_source),
    day1_input_source_label = dplyr::recode(day1_input_source_raw, !!!day1_source_label_map, .default = day1_input_source_raw)
  ) %>%
  filter(
    !is.na(y),
    prediction_day %in% c(1L, 2L),
    is.finite(vote_pct),
    !is.na(treatment),
    prior_adjustment_raw == "country_x_inpatient",
    (prediction_day == 1L) | (prediction_day == 2L & day1_input_source_raw == "predicted_day1")
  ) %>%
  mutate(
    treatment = factor(treatment, levels = treatment_order),
    prior_adjustment_label = factor(prior_adjustment_label, levels = unname(prior_label_map)),
    day1_input_source_label = factor(day1_input_source_label, levels = unname(day1_source_label_map)),
    prediction_day_label = factor(prediction_day_label, levels = c("Day 1", "Day 2"))
  )

if (nrow(d_all) == 0) stop("No valid rows available after cleaning/filtering.")

# Build day-specific subsets used for Day 1 vs Day 2 summary outputs.
d_day1 <- d_all %>% filter(prediction_day == 1L)
d_day2 <- d_all %>% filter(prediction_day == 2L)
if (nrow(d_day1) == 0) stop("No valid Day 1 rows available after cleaning/filtering.")
if (nrow(d_day2) == 0) stop("No valid Day 2 rows available after cleaning/filtering.")

# Models ----
fit_model_safe <- function(dat) {
  has_prior_term <- dplyr::n_distinct(dat$prior_adjustment_label) > 1

  f <- if (nlevels(dat$treatment) > 1 && has_prior_term) {
    y ~ vote_pct * treatment + prior_adjustment_label
  } else if (nlevels(dat$treatment) > 1 && !has_prior_term) {
    y ~ vote_pct * treatment
  } else if (nlevels(dat$treatment) <= 1 && has_prior_term) {
    y ~ vote_pct + prior_adjustment_label
  } else {
    y ~ vote_pct
  }

  tryCatch(
    suppressWarnings(glm(f, data = dat, family = binomial())),
    error = function(e) NULL
  )
}

fit_model_overall_safe <- function(dat) {
  has_prior_term <- dplyr::n_distinct(dat$prior_adjustment_label) > 1
  f <- if (has_prior_term) {
    y ~ vote_pct + prior_adjustment_label
  } else {
    y ~ vote_pct
  }

  tryCatch(
    suppressWarnings(glm(f, data = dat, family = binomial())),
    error = function(e) NULL
  )
}

compute_treatment_stats <- function(m, dat, delta_pp = 1) {
  levs <- levels(dat$treatment)

  slope_log_odds_per_1pp <- map_dbl(levs, function(lv) {
    new1 <- data.frame(
      vote_pct = 1,
      treatment = factor(lv, levels = levs),
      prior_adjustment_label = factor(levels(dat$prior_adjustment_label)[1], levels = levels(dat$prior_adjustment_label))
    )
    new0 <- new1
    new0$vote_pct <- 0
    lp1 <- predict(m, newdata = new1, type = "link")
    lp0 <- predict(m, newdata = new0, type = "link")
    as.numeric(lp1 - lp0)
  })

  q <- dat$vote_fraction
  q_plus <- pmin(q + delta_pp / 100, 1)
  dat_plus <- dat %>% mutate(vote_pct = q_plus * 100)

  p0 <- predict(m, newdata = dat, type = "response")
  p1 <- predict(m, newdata = dat_plus, type = "response")
  diff <- p1 - p0

  ame_by_treat <- tapply(diff, dat$treatment, mean, na.rm = TRUE)

  tibble(
    Treatment = levs,
    `Log-Odds Change per +1% Vote` = slope_log_odds_per_1pp,
    `Odds Ratio per +1% Vote` = exp(slope_log_odds_per_1pp),
    `Absolute Risk Change per +1% Vote (pp)` = as.numeric(ame_by_treat) * 100
  )
}

compute_overall_stats <- function(m, dat, delta_pp = 1) {
  q <- dat$vote_fraction
  q_plus <- pmin(q + delta_pp / 100, 1)
  dat_plus <- dat %>% mutate(vote_pct = q_plus * 100)

  p0 <- predict(m, newdata = dat, type = "response")
  p1 <- predict(m, newdata = dat_plus, type = "response")
  diff <- p1 - p0

  tibble(
    Treatment = "Overall",
    `Log-Odds Change per +1% Vote` = as.numeric(coef(m)["vote_pct"]),
    `Odds Ratio per +1% Vote` = exp(as.numeric(coef(m)["vote_pct"])),
    `Absolute Risk Change per +1% Vote (pp)` = mean(diff, na.rm = TRUE) * 100
  )
}

set.seed(20260226)
B <- 500L

build_day_effect_stats <- function(dat, day_label, B_boot = B) {
  m_treat <- fit_model_safe(dat)
  if (is.null(m_treat)) stop("Treatment-specific model failed to fit for ", day_label, ".")

  m_overall <- fit_model_overall_safe(dat)
  if (is.null(m_overall)) stop("Overall model failed to fit for ", day_label, ".")

  point_stats <- bind_rows(
    compute_overall_stats(m_overall, dat, delta_pp = 1),
    compute_treatment_stats(m_treat, dat, delta_pp = 1)
  )

  clusters <- split(dat, dat$patient_id)
  cluster_ids <- names(clusters)
  if (!length(cluster_ids)) stop("No patient clusters available for bootstrap in ", day_label, ".")

  boot_list <- purrr::map(seq_len(B_boot), function(b) {
    sampled_ids <- sample(cluster_ids, size = length(cluster_ids), replace = TRUE)
    boot_dat <- bind_rows(clusters[sampled_ids])

    mt <- fit_model_safe(boot_dat)
    mo <- fit_model_overall_safe(boot_dat)
    if (is.null(mt) || is.null(mo)) return(NULL)

    bind_rows(
      compute_overall_stats(mo, boot_dat, delta_pp = 1),
      compute_treatment_stats(mt, boot_dat, delta_pp = 1)
    ) %>%
      mutate(.bootstrap = b)
  })

  boot_stats <- bind_rows(compact(boot_list))
  if (nrow(boot_stats) == 0) stop("Bootstrap models failed for all resamples in ", day_label, ".")

  ci_stats <- boot_stats %>%
    group_by(Treatment) %>%
    summarise(
      `Absolute Risk Change CI Low (pp)` = quantile(`Absolute Risk Change per +1% Vote (pp)`, 0.025, na.rm = TRUE),
      `Absolute Risk Change CI High (pp)` = quantile(`Absolute Risk Change per +1% Vote (pp)`, 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  observed_stats <- bind_rows(
    dat %>%
      summarise(
        Treatment = "Overall",
        `Observed Treatment Rate` = mean(y, na.rm = TRUE)
      ),
    dat %>%
      group_by(treatment) %>%
      summarise(
        Treatment = as.character(first(treatment)),
        `Observed Treatment Rate` = mean(y, na.rm = TRUE),
        .groups = "drop"
      )
  )

  point_stats %>%
    left_join(ci_stats, by = "Treatment") %>%
    left_join(observed_stats, by = "Treatment") %>%
    mutate(
      `Prediction Day` = day_label,
      Treatment = factor(Treatment, levels = c("Overall", treatment_order))
    ) %>%
    arrange(Treatment)
}

effect_stats_day1_raw <- build_day_effect_stats(d_day1, "Day 1", B_boot = B)
effect_stats_day2_raw <- build_day_effect_stats(d_day2, "Day 2", B_boot = B)

effect_stats_raw <- bind_rows(effect_stats_day1_raw, effect_stats_day2_raw) %>%
  mutate(Treatment = factor(Treatment, levels = c("Overall", treatment_order))) %>%
  arrange(Treatment, `Prediction Day`)

observed_wide <- effect_stats_raw %>%
  transmute(
    Treatment,
    `Prediction Day`,
    `Observed treatment rate` = percent(`Observed Treatment Rate`, accuracy = 0.1)
  ) %>%
  tidyr::pivot_wider(
    names_from = `Prediction Day`,
    values_from = `Observed treatment rate`
  ) %>%
  rename(
    `Observed treatment rate (Day 1, %)` = `Day 1`,
    `Observed treatment rate (Day 2, %)` = `Day 2`
  )

ame_wide <- effect_stats_raw %>%
  transmute(
    Treatment,
    `Prediction Day`,
    `Absolute risk change per +1% vote` = sprintf(
      "%.3f (%.3f to %.3f)",
      `Absolute Risk Change per +1% Vote (pp)`,
      `Absolute Risk Change CI Low (pp)`,
      `Absolute Risk Change CI High (pp)`
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = `Prediction Day`,
    values_from = `Absolute risk change per +1% vote`
  ) %>%
  rename(
    `Absolute risk change per +1% vote (Day 1, percentage points; 95% CI)` = `Day 1`,
    `Absolute risk change per +1% vote (Day 2, percentage points; 95% CI)` = `Day 2`
  )

effect_stats_print <- tibble(Treatment = factor(c("Overall", treatment_order), levels = c("Overall", treatment_order))) %>%
  left_join(observed_wide, by = "Treatment") %>%
  left_join(ame_wide, by = "Treatment") %>%
  mutate(Treatment = as.character(Treatment)) %>%
  select(
    Treatment,
    `Observed treatment rate (Day 1, %)`,
    `Observed treatment rate (Day 2, %)`,
    `Absolute risk change per +1% vote (Day 1, percentage points; 95% CI)`,
    `Absolute risk change per +1% vote (Day 2, percentage points; 95% CI)`
  )

# Day 1 vs Day 2 curve pipeline (exact vote semantics; prior-only marginalisation) ----
# Important: `votes_above_threshold` is treated as a fraction in [0, 1].
d_overlay_exact_all <- d_all %>%
  transmute(
    patient_id,
    prediction_day = prediction_day,
    prediction_day_label = prediction_day_label,
    y = y,
    prior_adjustment = factor(as.character(prior_adjustment_label)),
    treatment_outcome = factor(as.character(treatment), levels = treatment_order),
    votes_above_threshold = vote_fraction,
    x1pp = votes_above_threshold / 0.01
  )

fit_overlay_exact_safe <- function(dat) {
  has_prior_term <- dplyr::n_distinct(dat$prior_adjustment) > 1
  f <- if (nlevels(dat$treatment_outcome) > 1 && has_prior_term) {
    y ~ x1pp * treatment_outcome + prior_adjustment
  } else if (nlevels(dat$treatment_outcome) > 1 && !has_prior_term) {
    y ~ x1pp * treatment_outcome
  } else if (nlevels(dat$treatment_outcome) <= 1 && has_prior_term) {
    y ~ x1pp + prior_adjustment
  } else {
    y ~ x1pp
  }

  tryCatch(
    suppressWarnings(glm(f, data = dat, family = binomial())),
    error = function(e) NULL
  )
}

fit_overlay_exact_overall_safe <- function(dat) {
  has_prior_term <- dplyr::n_distinct(dat$prior_adjustment) > 1
  f <- if (has_prior_term) {
    y ~ x1pp + prior_adjustment
  } else {
    y ~ x1pp
  }

  tryCatch(
    suppressWarnings(glm(f, data = dat, family = binomial())),
    error = function(e) NULL
  )
}

votes_grid_exact <- seq(0, 1, by = 0.01)

build_exact_curves_for_day <- function(dat_day_exact) {
  if (nrow(dat_day_exact) == 0) return(NULL)

  dat_day_exact <- dat_day_exact %>%
    mutate(
      prior_adjustment = factor(prior_adjustment, levels = unique(as.character(prior_adjustment))),
      treatment_outcome = factor(treatment_outcome, levels = treatment_order)
    )

  m_treat_exact <- fit_overlay_exact_safe(dat_day_exact)
  m_overall_exact <- fit_overlay_exact_overall_safe(dat_day_exact)
  if (is.null(m_treat_exact) || is.null(m_overall_exact)) return(NULL)

  prior_w_overall <- dat_day_exact %>%
    count(prior_adjustment) %>%
    mutate(w = n / sum(n)) %>%
    select(prior_adjustment, w)

  prior_w_by_treat <- dat_day_exact %>%
    count(treatment_outcome, prior_adjustment) %>%
    group_by(treatment_outcome) %>%
    mutate(w = n / sum(n)) %>%
    ungroup() %>%
    select(treatment_outcome, prior_adjustment, w)

  grid_overall <- expand.grid(
    votes_above_threshold = votes_grid_exact,
    prior_adjustment = levels(dat_day_exact$prior_adjustment),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      prior_adjustment = factor(prior_adjustment, levels = levels(dat_day_exact$prior_adjustment)),
      x1pp = votes_above_threshold / 0.01
    ) %>%
    mutate(
      p = predict(m_overall_exact, newdata = ., type = "response")
    )

  overall_curve <- grid_overall %>%
    left_join(prior_w_overall, by = "prior_adjustment") %>%
    group_by(votes_above_threshold) %>%
    summarise(p = sum(p * w), .groups = "drop")

  grid_treat <- expand.grid(
    votes_above_threshold = votes_grid_exact,
    treatment_outcome = levels(dat_day_exact$treatment_outcome),
    prior_adjustment = levels(dat_day_exact$prior_adjustment),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      treatment_outcome = factor(treatment_outcome, levels = levels(dat_day_exact$treatment_outcome)),
      prior_adjustment = factor(prior_adjustment, levels = levels(dat_day_exact$prior_adjustment)),
      x1pp = votes_above_threshold / 0.01
    ) %>%
    mutate(
      p = predict(m_treat_exact, newdata = ., type = "response")
    )

  treat_curve <- grid_treat %>%
    left_join(prior_w_by_treat, by = c("treatment_outcome", "prior_adjustment")) %>%
    group_by(treatment_outcome, votes_above_threshold) %>%
    summarise(p = sum(p * w), .groups = "drop")

  list(
    overall_curve = overall_curve,
    treat_curve = treat_curve
  )
}

build_overall_curve_boot_ci_for_day <- function(dat_day_exact, B_curve = B) {
  if (nrow(dat_day_exact) == 0) return(NULL)

  dat_day_exact <- dat_day_exact %>%
    mutate(
      prior_adjustment = factor(prior_adjustment, levels = unique(as.character(prior_adjustment)))
    )

  clusters_day <- split(dat_day_exact, dat_day_exact$patient_id)
  cluster_ids_day <- names(clusters_day)
  if (!length(cluster_ids_day)) return(NULL)

  boot_curves <- purrr::map(seq_len(B_curve), function(b) {
    sampled_ids <- sample(cluster_ids_day, size = length(cluster_ids_day), replace = TRUE)
    boot_dat <- bind_rows(clusters_day[sampled_ids]) %>%
      mutate(prior_adjustment = factor(prior_adjustment, levels = levels(dat_day_exact$prior_adjustment)))

    m_boot <- fit_overlay_exact_overall_safe(boot_dat)
    if (is.null(m_boot)) return(NULL)

    prior_w_boot <- boot_dat %>%
      count(prior_adjustment) %>%
      mutate(w = n / sum(n)) %>%
      select(prior_adjustment, w)

    grid_boot <- expand.grid(
      votes_above_threshold = votes_grid_exact,
      prior_adjustment = levels(dat_day_exact$prior_adjustment),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    ) %>%
      as_tibble() %>%
      mutate(
        prior_adjustment = factor(prior_adjustment, levels = levels(dat_day_exact$prior_adjustment)),
        x1pp = votes_above_threshold / 0.01
      ) %>%
      mutate(
        p = predict(m_boot, newdata = ., type = "response")
      )

    grid_boot %>%
      left_join(prior_w_boot, by = "prior_adjustment") %>%
      group_by(votes_above_threshold) %>%
      summarise(p = sum(p * w), .groups = "drop") %>%
      mutate(.bootstrap = b)
  })

  bind_rows(compact(boot_curves))
}

day_levels_present <- d_overlay_exact_all %>%
  distinct(prediction_day_label) %>%
  pull(prediction_day_label) %>%
  as.character()

exact_curves_by_day <- purrr::map(
  day_levels_present,
  function(day_lab) {
    dat_day <- d_overlay_exact_all %>% filter(as.character(prediction_day_label) == day_lab)
    curves <- build_exact_curves_for_day(dat_day)
    if (is.null(curves)) return(NULL)

    list(
      day_label = day_lab,
      overall_curve = curves$overall_curve %>% mutate(`Prediction Day` = day_lab),
      treat_curve = curves$treat_curve %>% mutate(`Prediction Day` = day_lab)
    )
  }
) %>% compact()

if (!length(exact_curves_by_day)) stop("Failed to build Day 1/Day 2 exact curves.")

overall_curve_exact_by_day <- bind_rows(purrr::map(exact_curves_by_day, "overall_curve"))
treat_curve_exact_by_day <- bind_rows(purrr::map(exact_curves_by_day, "treat_curve"))

overall_curve_boot_exact_by_day <- purrr::map(
  day_levels_present,
  function(day_lab) {
    dat_day <- d_overlay_exact_all %>% filter(as.character(prediction_day_label) == day_lab)
    build_overall_curve_boot_ci_for_day(dat_day, B_curve = B) %>%
      mutate(`Prediction Day` = day_lab)
  }
) %>% bind_rows()

overall_curve_ci_exact_by_day <- overall_curve_boot_exact_by_day %>%
  group_by(`Prediction Day`, votes_above_threshold) %>%
  summarise(
    p_ci_low = quantile(p, 0.025, na.rm = TRUE),
    p_ci_high = quantile(p, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

overall_curve_exact_with_ci <- overall_curve_exact_by_day %>%
  left_join(overall_curve_ci_exact_by_day, by = c("Prediction Day", "votes_above_threshold")) %>%
  mutate(
    `Prediction Day` = factor(`Prediction Day`, levels = c("Day 1", "Day 2"))
  )

# Print-ready curve objects for saving
curve_plot_df <- treat_curve_exact_by_day %>%
  transmute(
    Treatment = factor(as.character(treatment_outcome), levels = treatment_order),
    `Prediction Day` = factor(`Prediction Day`, levels = c("Day 1", "Day 2")),
    `Vote Fraction Above Threshold (%)` = 100 * votes_above_threshold,
    `Estimated Treatment Probability (%)` = 100 * p
  ) %>%
  arrange(Treatment, `Prediction Day`, `Vote Fraction Above Threshold (%)`)

overlay_curve_exact_print <- overall_curve_exact_with_ci %>%
  transmute(
    `Prediction Day` = factor(`Prediction Day`, levels = c("Day 1", "Day 2")),
    `Vote Fraction Above Threshold (%)` = 100 * votes_above_threshold,
    `Estimated Treatment Probability (%)` = 100 * p,
    `95% CI Low (%)` = 100 * p_ci_low,
    `95% CI High (%)` = 100 * p_ci_high
  ) %>%
  arrange(`Prediction Day`, `Vote Fraction Above Threshold (%)`)

# Empirical binned points for plotting (by day and treatment) ----
bin_width_pct <- 5
breaks_pct <- seq(0, 100, by = bin_width_pct)

binned_points_df <- d_all %>%
  mutate(
    vote_bin = cut(vote_pct, breaks = breaks_pct, include.lowest = TRUE, right = TRUE),
    vote_bin_mid = bin_width_pct * floor(vote_pct / bin_width_pct) + bin_width_pct / 2,
    vote_bin_mid = pmin(vote_bin_mid, 100)
  ) %>%
  group_by(treatment, prediction_day_label, vote_bin_mid) %>%
  summarise(
    `Rows (n)` = n(),
    `Observed Treatment Rate (%)` = 100 * mean(y, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    Treatment = factor(as.character(treatment), levels = treatment_order),
    `Prediction Day` = factor(as.character(prediction_day_label), levels = c("Day 1", "Day 2")),
    `Vote Fraction Above Threshold (%)` = vote_bin_mid,
    `Observed Treatment Rate (%)`,
    `Rows (n)`
  ) %>%
  arrange(Treatment, `Prediction Day`, `Vote Fraction Above Threshold (%)`)

# Plots ----
plot_curve <- curve_plot_df %>%
  rename(
    VotePct = `Vote Fraction Above Threshold (%)`,
    PredictedPct = `Estimated Treatment Probability (%)`,
    PredictionDay = `Prediction Day`
  )

plot_points <- binned_points_df %>%
  rename(
    VotePct = `Vote Fraction Above Threshold (%)`,
    ObservedPct = `Observed Treatment Rate (%)`,
    PredictionDay = `Prediction Day`
  )

p_calibration <- ggplot() +
  geom_line(
    data = plot_curve,
    aes(x = VotePct, y = PredictedPct, color = PredictionDay),
    linewidth = 1.0,
    alpha = 0.95
  ) +
  geom_point(
    data = plot_points,
    aes(x = VotePct, y = ObservedPct, color = PredictionDay, shape = PredictionDay),
    size = 2.4,
    alpha = 0.75,
    stroke = 0.2
  ) +
  facet_wrap(~ Treatment, ncol = 3, scales = "fixed") +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%")
  ) +
  scale_color_manual(values = c("Day 1" = "#0B5563", "Day 2" = "#C65D3B")) +
  scale_shape_manual(values = c("Day 1" = 16, "Day 2" = 17)) +
  labs(
    title = "Treatment Specific Probability vs Ensemble Vote Share (Day 1 vs Day 2)",
    subtitle = "Model estimated curves compared with observed treatment rates (points) in 5% vote-share bins.",
    x = "Ensemble Votes Above Threshold (%)",
    y = "Predicted / Observed Treatment Probability (%)",
    color = "Prediction Day",
    shape = "Prediction Day",
    caption = "Day 2 curves use feed forward predicted Day 1 treatment inputs; curves are marginalised over prior-adjustment strata for the fully adjusted (country, inpatient status, and treatment) setting."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EAECEF", linewidth = 0.35),
    strip.text = element_text(face = "bold", size = 10.5),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10.2, lineheight = 1.15),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

# Overlay curves: overall Day 1 vs Day 2 with 95% CI ----
overlay_y_range <- range(
  c(overall_curve_exact_with_ci$p_ci_low, overall_curve_exact_with_ci$p_ci_high),
  na.rm = TRUE,
  finite = TRUE
)
overlay_y_pad <- 0.03
overlay_y_limits <- c(
  max(0, overlay_y_range[1] - overlay_y_pad),
  min(1, overlay_y_range[2] + overlay_y_pad)
)

p_overlay <- ggplot(
  overall_curve_exact_with_ci,
  aes(x = votes_above_threshold, y = p, color = `Prediction Day`, fill = `Prediction Day`)
) +
  geom_ribbon(aes(ymin = p_ci_low, ymax = p_ci_high), alpha = 0.18, linewidth = 0, color = NA) +
  geom_line(linewidth = 1.15) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = overlay_y_limits,
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_color_manual(values = c("Day 1" = "#0B5563", "Day 2" = "#C65D3B")) +
  scale_fill_manual(values = c("Day 1" = "#0B5563", "Day 2" = "#C65D3B")) +
  labs(
    title = "Overall Predicted Treatment Probability by Ensemble Vote Share: Day 1 vs Day 2",
    subtitle = "Estimated treatment probability by vote share; shaded bands represent bootstrap 95% CIs. Day 2 estimates use feed-forward predicted Day-1 treatments.",
    caption = "Vote share denotes proportion of ensemble heads exceeding the context decision thresholds derived using country, inpatient and treatment prior prevalence adjustment.",
    x = "Voters Above Threshold",
    y = "Predicted Probability of Treatment",
    color = "Prediction Day",
    fill = "Prediction Day"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EAECEF", linewidth = 0.35),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10.2, lineheight = 1.15),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold")
  ) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1.3, alpha = 1)),
    fill = "none"
  )

effect_plot_df <- effect_stats_raw %>%
  filter(`Prediction Day` == "Day 2") %>%
  mutate(Treatment = factor(as.character(Treatment), levels = rev(c("Overall", treatment_order))))

p_effects <- ggplot(effect_plot_df, aes(y = Treatment)) +
  geom_vline(xintercept = 0, color = "#D0D7DE", linewidth = 0.5) +
  geom_segment(
    aes(
      x = `Absolute Risk Change CI Low (pp)`,
      xend = `Absolute Risk Change CI High (pp)`,
      yend = Treatment
    ),
    linewidth = 0.8,
    color = "#7A869A"
  ) +
  geom_point(
    aes(x = `Absolute Risk Change per +1% Vote (pp)`),
    size = 2.4,
    shape = 21,
    fill = "#0B5563",
    color = "white",
    stroke = 0.3
  ) +
  labs(
    title = "Change in Day 2 Treatment Probability per +1% Ensemble Vote",
    subtitle = "Absolute risk change (percentage points) from logistic models with patient-cluster bootstrap 95% confidence intervals.",
    x = "Absolute Risk Change per +1% Vote (pp)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    axis.title.x = element_text(face = "bold")
  )

# Save outputs ----
analysis_data_print_ready <- d_all %>%
  transmute(
    `Patient ID` = patient_id,
    `Patient Index` = patient_index,
    `Prediction Day` = as.character(prediction_day_label),
    `Treatment` = as.character(treatment),
    `Prior Adjustment` = as.character(prior_adjustment_label),
    `Day 1 Input Source` = as.character(day1_input_source_label),
    `Observed Treatment` = y,
    `Vote Fraction Above Threshold (%)` = round(vote_pct, 1),
    `Vote Count Above Threshold` = as.integer(round(vote_fraction * vote_denominator)),
    `Vote Denominator` = as.integer(round(vote_denominator))
  )

analysis_bundle <- list(
  metadata = list(
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    input_source = if (file.exists(input_bundle_path)) basename(input_bundle_path) else paste(basename(input_day1_rds_path), "+", basename(input_day2_rds_path)),
    bootstrap_reps = B,
    effect_summary_scope = "Combined Day 1 and Day 2",
    effect_summary_footnote = "Vote share denotes the proportion of ensemble heads above threshold; Day 2 uses feed-forward predicted Day 1 treatment inputs; confidence intervals are patient-cluster bootstrapped.",
    plot_comparison_scope = "Day 1 vs Day 2",
    prior_adjustment_scope = "Country x Inpatient Prior only",
    day2_input_source_scope = "Predicted Day 1 pathway only",
    bin_width_pct = bin_width_pct
  ),
  tables = list(
    effect_summary_print_ready = effect_stats_print,
    effect_summary_raw = effect_stats_raw,
    curve_data_print_ready = curve_plot_df,
    empirical_bin_points_print_ready = binned_points_df,
    overlay_curve_data_exact_print_ready = overlay_curve_exact_print
  ),
  data = list(
    analysis_data_print_ready = analysis_data_print_ready
  ),
  plots = list(
    overlay_curves = p_overlay,
    calibration_facets = p_calibration,
    effect_forest = p_effects
  )
)

saveRDS(analysis_bundle, file = file.path(eval_out_dir, "voters_pct_eval_bundle.rds"))
saveRDS(effect_stats_print, file = file.path(eval_out_dir, "voters_pct_effect_summary_print_ready.rds"))
write.csv(effect_stats_print, file = file.path(table_out_dir, "voters_pct_effect_summary_print_ready.csv"), row.names = FALSE)
saveRDS(curve_plot_df, file = file.path(eval_out_dir, "voters_pct_curve_data_print_ready.rds"))
saveRDS(binned_points_df, file = file.path(eval_out_dir, "voters_pct_empirical_bin_points_print_ready.rds"))
saveRDS(overlay_curve_exact_print, file = file.path(eval_out_dir, "voters_pct_overlay_curve_data_exact_print_ready.rds"))
saveRDS(analysis_data_print_ready, file = file.path(eval_out_dir, "voters_pct_analysis_data_print_ready.rds"))
saveRDS(p_overlay, file = file.path(eval_out_dir, "voters_pct_overlay_curve_plot.rds"))
saveRDS(p_calibration, file = file.path(eval_out_dir, "voters_pct_calibration_plot.rds"))
saveRDS(p_effects, file = file.path(eval_out_dir, "voters_pct_effect_forest_plot.rds"))

ggsave(
  filename = file.path(fig_out_dir, "voters_pct_overlay_curves.png"),
  plot = p_overlay,
  width = 10.5,
  height = 6.5,
  dpi = 320,
  bg = "white"
)
ggsave(
  filename = file.path(fig_out_dir, "voters_pct_overlay_curves.pdf"),
  plot = p_overlay,
  width = 10.5,
  height = 6.5,
  bg = "white"
)
ggsave(
  filename = file.path(fig_out_dir, "voters_pct_calibration_facets.png"),
  plot = p_calibration,
  width = 12,
  height = 8,
  dpi = 320,
  bg = "white"
)
ggsave(
  filename = file.path(fig_out_dir, "voters_pct_calibration_facets.pdf"),
  plot = p_calibration,
  width = 12,
  height = 8,
  bg = "white"
)
ggsave(
  filename = file.path(fig_out_dir, "voters_pct_effect_forest.png"),
  plot = p_effects,
  width = 9,
  height = 4.8,
  dpi = 320,
  bg = "white"
)
ggsave(
  filename = file.path(fig_out_dir, "voters_pct_effect_forest.pdf"),
  plot = p_effects,
  width = 9,
  height = 4.8,
  bg = "white"
)

# Print / display ----
message("Saved evaluation outputs -> ", eval_out_dir)
message("Saved tables -> ", table_out_dir)
message("Saved figures -> ", fig_out_dir)

print(effect_stats_print)
print(p_overlay)
print(p_calibration)
print(p_effects)
