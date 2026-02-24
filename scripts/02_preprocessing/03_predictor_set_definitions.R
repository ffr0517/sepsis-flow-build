# Predictor set definitions (unweighted) ----

source(here::here("R", "bootstrap.R"))

train_path <- file.path(PATHS$processed, "train.rds")
require_files(train_path, stage_id = "predictor_sets")
train_base <- readRDS(train_path)

id <- "label"
binary_outcome <- "outcome.binary"
linear_outcome <- "outcome.linear.2"
theta_outcome <- "outcome.theta"
oxy_sat <- "oxy.ra"

restricted_predictors <- c(
  "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
  "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long"
)

make_subset <- function(data, cols, factor_binary = FALSE, drop_binary = FALSE) {
  out <- data %>% dplyr::select(all_of(cols))

  if (factor_binary && binary_outcome %in% names(out)) {
    out[[binary_outcome]] <- factor(out[[binary_outcome]], levels = c(0, 1))
  }

  if (drop_binary && binary_outcome %in% names(out)) {
    out <- out %>% dplyr::select(-all_of(binary_outcome))
  }

  out
}

# Binary outcomes ----
final_bin <- make_subset(
  train_base,
  c(id, restricted_predictors, binary_outcome, "combination_id"),
  factor_binary = TRUE
)

final_oxy_bin <- make_subset(
  train_base,
  c(id, restricted_predictors, oxy_sat, binary_outcome, "combination_id"),
  factor_binary = TRUE
)

# Scale outcomes ----
final_lin <- make_subset(
  train_base,
  c(id, restricted_predictors, linear_outcome, binary_outcome, "combination_id"),
  drop_binary = TRUE
)

final_oxy_lin <- make_subset(
  train_base,
  c(id, restricted_predictors, oxy_sat, linear_outcome, binary_outcome, "combination_id"),
  drop_binary = TRUE
)

# Theta outcomes ----
final_theta <- make_subset(
  train_base,
  c(id, restricted_predictors, theta_outcome, binary_outcome, "combination_id"),
  drop_binary = TRUE
)

final_oxy_theta <- make_subset(
  train_base,
  c(id, restricted_predictors, oxy_sat, theta_outcome, binary_outcome, "combination_id"),
  drop_binary = TRUE
)

# Saving ----
write_rds(final_bin, file.path(PATHS$processed, "train_restricted_binary.rds"))
write_rds(final_oxy_bin, file.path(PATHS$processed, "train_restricted_+oxy_binary.rds"))
write_rds(final_lin, file.path(PATHS$processed, "train_restricted_linear.rds"))
write_rds(final_oxy_lin, file.path(PATHS$processed, "train_restricted_+oxy_linear.rds"))
write_rds(final_theta, file.path(PATHS$processed, "train_restricted_theta.rds"))
write_rds(final_oxy_theta, file.path(PATHS$processed, "train_restricted_+oxy_theta.rds"))

