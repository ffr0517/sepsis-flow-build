# Global settings ----
source(here::here("R", "bootstrap.R"))

library(rpart)
library(rpart.plot)
library(pROC)

save_dir <- PATHS$ensemble_workspace

# Setting outcome for this script ----
SELECTED_OUTCOME_VAR <- "LEVEL4_TREATMENTS_D2_SAFE_0"

OUTCOME_SYM <- rlang::sym(SELECTED_OUTCOME_VAR)

# Loading data ----
raw_train <- readRDS(file.path(PATHS$processed, "train.rds"))
raw_test <- readRDS(file.path(PATHS$processed, "test.rds"))
measures_df <- read.csv(file.path(PATHS$processed, "individual_measures.csv"))

# ADDING TRAINING DATA WITH PREDICTED VALUES FOR DAY ONE INTERVENTIONS ----
df3 <- readRDS(file.path(save_dir, "treatment_predicted_train.rds"))

treatment_vars <- c(
  "LEVEL1_TREATMENTS_D1_SAFE_0",
  "LEVEL2_TREATMENTS_D1_SAFE_0",
  "LEVEL3_TREATMENTS_D1_SAFE_0",
  "LEVEL4_TREATMENTS_D1_SAFE_0",
  "LEVEL5_TREATMENTS_D1_SAFE_0"
)

df3[treatment_vars] <- lapply(
  df3[treatment_vars],
  function(x) {
    as.integer(x == "yes")
  }
)

df3 <- df3 %>%
  left_join(
    measures_df %>% select(-all_of(treatment_vars)),
    by = c("label" = "Label")
  )

df3[[SELECTED_OUTCOME_VAR]] <- factor(
  df3[[SELECTED_OUTCOME_VAR]],
  levels = c(0, 1),
  labels = c("no", "yes")
)

# Joining ----
df <- raw_train %>%
  left_join(measures_df, by = c("label" = "Label"))

df <- df %>%
  mutate(
    country = case_when(
      grepl("^bd", site, ignore.case = TRUE) ~ "Bangladesh",
      grepl("^id", site, ignore.case = TRUE) ~ "Indonesia",
      grepl("^kh", site, ignore.case = TRUE) ~ "Cambodia",
      grepl("^la", site, ignore.case = TRUE) ~ "Laos",
      grepl("^vn", site, ignore.case = TRUE) ~ "Vietnam",
      TRUE ~ NA_character_
    )
  )

# Creating control strata ----
df$stratum <- interaction(
  df$country,
  df$ipdopd,
  drop = TRUE
)

# Removing clutter 
df2 <- df %>%
  select(-c(
    age.group,
    DEATHD1, DEATHD2,
    LEVEL1_TREATMENTS_D1, LEVEL1_TREATMENTS_D2,
    LEVEL2_TREATMENTS_D1, LEVEL2_TREATMENTS_D2,
    LEVEL3_TREATMENTS_D1, LEVEL3_TREATMENTS_D2,
    LEVEL4_TREATMENTS_D1, LEVEL4_TREATMENTS_D2,
    LEVEL5_TREATMENTS_D1, LEVEL5_TREATMENTS_D2,
    GENERIC_ORG_SUP, DISCHARGE_DIE,
    IPDOPD, FUCARE, FUADMYN, FUMEDYN, FUD2ORG,
    ENDAT, DCDAT, DCORG,
    days_to_discharge,
    inpatient_d1_discharge,
    safe_zero_outpatient,
    safe_zero_inpatient_d2,
    safe_zero_d1,
    safe_zero_d2,
    combination_id,
    country, 
        # part two vars
    ANG1, ANG2, CHI3L, CRP, IL10, IL1ra, IL6, IL8, PROC, STREM1, TNFR1,
    VEGFR1, enescbchb1, lblac, lbglu, supar, CXCl10
  ))

rm(raw_train);rm(df)

# Making outcome a factor ----
df2[[SELECTED_OUTCOME_VAR]] <- factor(
  df2[[SELECTED_OUTCOME_VAR]],
  levels = c(0, 1),
  labels = c("no", "yes")
)

# Making actual treatment vars numeric ----
df2[treatment_vars] <- lapply(
  df2[treatment_vars],
  function(x) {
    as.integer(x == "yes")
  }
)

# Creating enriched sampled controls + bootstrapped cases datasets ----
set.seed(GLOBALS$seed)

n_sets <- 120
ratio_controls_per_case <- 1

cases <- df2 %>% filter(.data[[SELECTED_OUTCOME_VAR]] == "yes")
controls <- df2 %>% filter(.data[[SELECTED_OUTCOME_VAR]] == "no")

n_cases <- nrow(cases)
n_controls_target <- ratio_controls_per_case * n_cases

# control stratum proportions 
stratum_props <- controls %>%
  count(stratum, name = "n_in_stratum") %>%
  mutate(prop = n_in_stratum / sum(n_in_stratum))

# allocate integer counts that sum exactly to n_controls_target
allocate_by_props <- function(props_df, total_n) {
  alloc <- props_df %>%
    mutate(raw = prop * total_n,
           n_floor = floor(raw),
           frac = raw - n_floor)
  
  remainder <- total_n - sum(alloc$n_floor)
  
  if (remainder > 0) {
    alloc <- alloc %>%
      arrange(desc(frac)) %>%
      mutate(n_add = if_else(row_number() <= remainder, 1L, 0L))
  } else {
    alloc <- alloc %>%
      mutate(n_add = 0L)
  }
  
  alloc %>%
    mutate(n_sample = n_floor + n_add) %>%
    select(stratum, n_sample)
}

stratum_targets <- allocate_by_props(stratum_props, n_controls_target)

make_one_dataset <- function() {
  sampled_cases <- cases %>%
    slice_sample(n = n_cases, replace = TRUE)
  
  sampled_controls <- controls %>%
    group_by(stratum) %>%
    group_modify(~ {
      s <- as.character(.y$stratum[1])
      idx <- match(s, stratum_targets$stratum)
      
      n_s <- if (is.na(idx)) 0L else as.integer(stratum_targets$n_sample[[idx]])
      
      if (is.na(n_s) || n_s <= 0) return(.x[0, ])
      n_s <- min(n_s, nrow(.x))
      
      slice_sample(.x, n = n_s, replace = FALSE)
    }) %>%
    ungroup()
  
  bind_rows(sampled_cases, sampled_controls)
}

datasets_120 <- purrr::map(seq_len(n_sets), ~ make_one_dataset())

names(datasets_120) <- paste0("d_", seq_along(datasets_120))

# min/max summary
tibble(
  dataset = names(datasets_120),
  n_cases = map_int(datasets_120, ~ sum(.x[[SELECTED_OUTCOME_VAR]] == "yes")),
  n_controls = map_int(datasets_120, ~ sum(.x[[SELECTED_OUTCOME_VAR]]  == "no"))
) %>% summarise(min_cases = min(n_cases), max_cases = max(n_cases),
                min_controls = min(n_controls), max_controls = max(n_controls))

# bootstrap summary
bootstrap_diag <- imap_dfr(datasets_120, function(d, name) {
  case_ids <- d %>%
    filter(.data[[SELECTED_OUTCOME_VAR]] == "yes") %>%
    pull(label)
  
  tibble(
    dataset = name,
    unique_frac = length(unique(case_ids)) / length(case_ids),
    dup_row_frac = mean(duplicated(case_ids)),
    max_multiplicity = max(table(case_ids))
  )
})

bootstrap_diag

# adding rows with predicted values 
datasets_120 <- purrr::map(
  datasets_120,
  function(d) {
    label_counts <- table(d$label)
    
    d3_expanded <- purrr::map_dfr(
      names(label_counts),
      function(lab) {
        rows <- df3[df3$label == lab, , drop = FALSE]
        rows[rep(seq_len(nrow(rows)), label_counts[[lab]]), , drop = FALSE]
      }
    )
    
    dplyr::bind_rows(d, d3_expanded) 
  }
)

# Creating cv objects ----
set.seed(GLOBALS$seed)
cv_120 <- imap(
  datasets_120,
  ~ vfold_cv(.x, v = 5, strata = SELECTED_OUTCOME_VAR)
)

# Setting seed for model runners ----
set.seed(GLOBALS$seed)

# LINEAR LASSO ----
run_linear_lasso_one <- function(
    dataset_key,
    datasets,
    cvs,
    vi_metric = c("roc_auc", "pr_auc"),
    vi_nsim = 20
) {
  vi_metric <- match.arg(vi_metric)
  
  dataset_idx <- match(dataset_key, names(datasets))
  df_x <- datasets[[dataset_idx]]
  
  model_vars <- c(
    # clinical features
    "age.months", "sex", "adm.recent", "wfaz", "cidysymp", "not.alert",
    "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra",
    
    # day one treatment
    "LEVEL1_TREATMENTS_D1_SAFE_0", "LEVEL2_TREATMENTS_D1_SAFE_0", 
    "LEVEL3_TREATMENTS_D1_SAFE_0", "LEVEL4_TREATMENTS_D1_SAFE_0",
    "LEVEL5_TREATMENTS_D1_SAFE_0"
  )
  
  outcome_formula <- as.formula(
    paste(
      SELECTED_OUTCOME_VAR,
      "~ age.months + sex + adm.recent + wfaz + cidysymp + not.alert +",
      "hr.all + rr.all + envhtemp + crt.long + oxy.ra +",
      "LEVEL1_TREATMENTS_D1_SAFE_0 + LEVEL2_TREATMENTS_D1_SAFE_0 +",
      "LEVEL3_TREATMENTS_D1_SAFE_0 + LEVEL4_TREATMENTS_D1_SAFE_0 +",
      "LEVEL5_TREATMENTS_D1_SAFE_0"
    )
  )
  
  df_x_rec <- recipe(outcome_formula, data = df_x) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_indicate_na(all_predictors()) %>%
    
    step_mutate(
      oxy.ra = log(oxy.ra + 1e-6),
      age_x_rr = age.months * rr.all,
      age_x_hr = age.months * hr.all
    ) %>%
    
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())
  
  mod_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet")
  
  df_x_wf <- workflow() %>%
    add_recipe(df_x_rec) %>%
    add_model(mod_spec)

  df_x_res <- tune_grid(
    df_x_wf,
    resamples = cvs[[dataset_idx]],
    grid = 50,
    metrics = metric_set(roc_auc, pr_auc, mn_log_loss),
    control = control_grid(save_pred = TRUE)
  )
  
  metrics_all <- collect_metrics(df_x_res, summarize = TRUE)
  
  best_row <- metrics_all %>%
    filter(.metric == "mn_log_loss") %>%
    arrange(mean, std_err) %>%
    head(1)
  
  best_config <- best_row$.config
  best_penalty <- best_row %>% select(penalty)
  
  df_x_final_wf <- finalize_workflow(df_x_wf, best_penalty)
  df_x_final_fit <- workflows::fit(df_x_final_wf, data = df_x)
  
  vip_metric_fn <- switch(
    vi_metric,
    roc_auc = function(truth, estimate) {
      yardstick::roc_auc_vec(truth = truth, estimate = estimate, event_level = "second")
    },
    pr_auc = function(truth, estimate) {
      yardstick::pr_auc_vec(truth = truth, estimate = estimate, event_level = "second")
    }
  )
  
  df_x_varimp <- vip::vi_permute(
    object = df_x_final_fit,
    feature_names = setdiff(names(df_x), SELECTED_OUTCOME_VAR),
    train = df_x,
    target = SELECTED_OUTCOME_VAR,
    metric = vip_metric_fn,
    pred_wrapper = function(object, newdata) {
      predict(object, new_data = newdata, type = "prob")$.pred_yes
    },
    nsim = vi_nsim,
    smaller_is_better = FALSE
  ) %>%
    filter(Variable %in% model_vars) %>%
    arrange(desc(Importance))
  
  test_idx <- sample(setdiff(seq_along(datasets), dataset_idx), 1)
  df_test <- datasets[[test_idx]]
  
  df_test_preds <- predict(df_x_final_fit, new_data = df_test, type = "prob") %>%
    bind_cols(df_test %>% select(.data[[SELECTED_OUTCOME_VAR]]))
  
  test_roc_auc <- roc_auc(df_test_preds, .data[[SELECTED_OUTCOME_VAR]], .pred_yes, event_level = "second")$.estimate
  test_log_loss <- mn_log_loss(df_test_preds, .data[[SELECTED_OUTCOME_VAR]], .pred_yes, event_level = "second")$.estimate
  test_pr_auc <- pr_auc(df_test_preds, .data[[SELECTED_OUTCOME_VAR]], .pred_yes, event_level = "second")$.estimate
  
  head_obj <- list(model = df_x_final_fit)
  
  list(
    dataset_key = dataset_key,
    dataset_idx = dataset_idx,
    best_penalty = best_penalty,
    best_config = best_config,
    test_roc_auc = test_roc_auc,
    test_pr_auc = test_pr_auc,
    test_log_loss = test_log_loss,
    head = head_obj,
    varimp = df_x_varimp,
    varimp_metric = vi_metric,
    varimp_nsim = vi_nsim,
    tuning = df_x_res
  )
}

dataset_keys <- names(datasets_120)[1:40]

results_1_40 <- purrr::set_names(
  purrr::map(dataset_keys, ~ run_linear_lasso_one(.x, datasets_120, cv_120)),
  dataset_keys
)

purrr::walk(results_1_40, ~ {
  head_name <- paste0("head_", .x$dataset_idx)
  varimp_name <- paste0("df_", .x$dataset_idx, "_varimp")
  
  assign(head_name, .x$head, envir = .GlobalEnv)
  assign(varimp_name, .x$varimp, envir = .GlobalEnv)
})

# SPLINE LASSO ----
run_spline_lasso_one <- function(
    dataset_key,
    datasets,
    cvs,
    deg_free = 4,
    vi_metric = c("roc_auc", "pr_auc"),
    vi_nsim = 20
) {
  vi_metric <- match.arg(vi_metric)
  
  dataset_idx <- match(dataset_key, names(datasets))
  df_x <- datasets[[dataset_idx]]
  
  model_vars <- c(
    "age.months", "sex", "adm.recent", "wfaz", "cidysymp", "not.alert",
    "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra",
    "LEVEL1_TREATMENTS_D1_SAFE_0", "LEVEL2_TREATMENTS_D1_SAFE_0",
    "LEVEL3_TREATMENTS_D1_SAFE_0", "LEVEL4_TREATMENTS_D1_SAFE_0",
    "LEVEL5_TREATMENTS_D1_SAFE_0"
  )
  
  outcome_formula <- as.formula(
    paste(
      SELECTED_OUTCOME_VAR,
      "~ age.months + sex + adm.recent + wfaz + cidysymp + not.alert +",
      "hr.all + rr.all + envhtemp + crt.long + oxy.ra +",
      "LEVEL1_TREATMENTS_D1_SAFE_0 + LEVEL2_TREATMENTS_D1_SAFE_0 +",
      "LEVEL3_TREATMENTS_D1_SAFE_0 + LEVEL4_TREATMENTS_D1_SAFE_0 +",
      "LEVEL5_TREATMENTS_D1_SAFE_0"
    )
  )
  
  df_x_rec <- recipe(outcome_formula, data = df_x) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_indicate_na(all_predictors()) %>%
    step_mutate(
      oxy.ra = log(oxy.ra + 1e-6),
      age_x_rr = age.months * rr.all,
      age_x_hr = age.months * hr.all
    ) %>%
    step_zv(all_predictors()) %>%
    step_ns(age.months, deg_free = deg_free) %>%
    step_ns(hr.all, deg_free = deg_free) %>%
    step_ns(rr.all, deg_free = deg_free) %>%
    step_ns(oxy.ra, deg_free = deg_free) %>%
    step_ns(envhtemp, deg_free = deg_free) %>%
    step_ns(wfaz, deg_free = deg_free) %>%
    step_ns(cidysymp, deg_free = deg_free) %>%
    step_normalize(all_numeric_predictors())
  
  mod_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet")
  
  df_x_wf <- workflow() %>%
    add_recipe(df_x_rec) %>%
    add_model(mod_spec)

  df_x_res <- tune_grid(
    df_x_wf,
    resamples = cvs[[dataset_idx]],
    grid = 50,
    metrics = metric_set(roc_auc, pr_auc, mn_log_loss),
    control = control_grid(save_pred = TRUE)
  )
  
  metrics_all <- collect_metrics(df_x_res, summarize = TRUE)
  
  best_row <- metrics_all %>%
    filter(.metric == "mn_log_loss") %>%
    arrange(mean, std_err) %>%
    head(1)
  
  best_config <- best_row$.config
  best_penalty <- best_row %>% select(penalty)
  
  df_x_final_wf <- finalize_workflow(df_x_wf, best_penalty)
  df_x_final_fit <- workflows::fit(df_x_final_wf, data = df_x)
  
  vip_metric_fn <- switch(
    vi_metric,
    roc_auc = function(truth, estimate) {
      yardstick::roc_auc_vec(truth = truth, estimate = estimate, event_level = "second")
    },
    pr_auc = function(truth, estimate) {
      yardstick::pr_auc_vec(truth = truth, estimate = estimate, event_level = "second")
    }
  )
  
  df_x_varimp_raw <- vip::vi_permute(
    object = df_x_final_fit,
    feature_names = setdiff(names(df_x), SELECTED_OUTCOME_VAR),
    train = df_x,
    target = SELECTED_OUTCOME_VAR,
    metric = vip_metric_fn,
    pred_wrapper = function(object, newdata) {
      predict(object, new_data = newdata, type = "prob")$.pred_yes
    },
    nsim = vi_nsim,
    smaller_is_better = FALSE
  )
  
  df_x_varimp <- df_x_varimp_raw %>%
    mutate(
      Variable_group = sub("_ns_.*$", "", Variable)
    ) %>%
    group_by(Variable_group) %>%
    summarise(
      Importance = mean(Importance),
      StdError = if ("StdError" %in% names(cur_data())) {
        mean(StdError)
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    rename(Variable = Variable_group) %>%
    filter(Variable %in% model_vars) %>%
    arrange(desc(Importance))
  
  test_idx <- sample(setdiff(seq_along(datasets), dataset_idx), 1)
  df_test <- datasets[[test_idx]]
  
  df_test_preds <- predict(df_x_final_fit, new_data = df_test, type = "prob") %>%
    bind_cols(df_test %>% select(.data[[SELECTED_OUTCOME_VAR]]))
  
  test_roc_auc <- roc_auc(
    df_test_preds,
    .data[[SELECTED_OUTCOME_VAR]],
    .pred_yes,
    event_level = "second"
  )$.estimate
  
  test_pr_auc <- pr_auc(
    df_test_preds,
    .data[[SELECTED_OUTCOME_VAR]],
    .pred_yes,
    event_level = "second"
  )$.estimate
  
  test_log_loss <- mn_log_loss(
    df_test_preds,
    .data[[SELECTED_OUTCOME_VAR]],
    .pred_yes,
    event_level = "second"
  )$.estimate
  
  coef_tbl <- broom::tidy(
    extract_fit_parsnip(df_x_final_fit),
    return_zeros = FALSE
  )
  
  spline_coefs <- coef_tbl %>%
    filter(grepl("_ns_", term)) %>%
    mutate(
      variable = sub("_ns_.*$", "", term)
    )
  
  head_obj <- list(
    model = df_x_final_fit,
    coef_all = coef_tbl,
    coef_spline = spline_coefs
  )
  
  list(
    dataset_key = dataset_key,
    dataset_idx = dataset_idx,
    best_penalty = best_penalty,
    best_config = best_config,
    test_roc_auc = test_roc_auc,
    test_pr_auc = test_pr_auc,
    test_log_loss = test_log_loss,
    head = head_obj,
    varimp = df_x_varimp,
    varimp_metric = vi_metric,
    varimp_nsim = vi_nsim,
    tuning = df_x_res
  )
}

dataset_keys <- names(datasets_120)[41:80]

results_41_80 <- purrr::set_names(
  purrr::map(dataset_keys, ~ run_spline_lasso_one(.x, datasets_120, cv_120)),
  dataset_keys
)

purrr::walk(results_41_80, ~ {
  head_name <- paste0("head_", .x$dataset_idx)
  varimp_name <- paste0("df_", .x$dataset_idx, "_varimp")
  
  assign(head_name, .x$head, envir = .GlobalEnv)
  assign(varimp_name, .x$varimp, envir = .GlobalEnv)
})

# CART ----
run_cart_one <- function(
    dataset_key,
    datasets,
    cvs,
    cart_grid = NULL,
    vi_metric = c("roc_auc", "pr_auc"),
    vi_nsim = 20
) {
  vi_metric <- match.arg(vi_metric)
  
  dataset_idx <- match(dataset_key, names(datasets))
  df_x <- datasets[[dataset_idx]]
  
  model_vars <- c(
    # clinical features
    "age.months", "sex", "adm.recent", "wfaz", "cidysymp", "not.alert",
    "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra",
    
    # day one treatment
    "LEVEL1_TREATMENTS_D1_SAFE_0", "LEVEL2_TREATMENTS_D1_SAFE_0", 
    "LEVEL3_TREATMENTS_D1_SAFE_0", "LEVEL4_TREATMENTS_D1_SAFE_0",
    "LEVEL5_TREATMENTS_D1_SAFE_0"
  )
  
  outcome_formula <- as.formula(
    paste(
      SELECTED_OUTCOME_VAR,
      "~ age.months + sex + adm.recent + wfaz + cidysymp + not.alert +",
      "hr.all + rr.all + envhtemp + crt.long + oxy.ra +",
      "LEVEL1_TREATMENTS_D1_SAFE_0 + LEVEL2_TREATMENTS_D1_SAFE_0 +",
      "LEVEL3_TREATMENTS_D1_SAFE_0 + LEVEL4_TREATMENTS_D1_SAFE_0 +",
      "LEVEL5_TREATMENTS_D1_SAFE_0"
    )
  )
  
  df_x_rec <- recipe(outcome_formula, data = df_x) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_indicate_na(all_predictors()) %>%
    step_mutate(
      oxy.ra = log(oxy.ra + 1e-6),
      age_x_rr = age.months * rr.all,
      age_x_hr = age.months * hr.all
    ) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())
  
  mod_spec <- decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
  ) %>%
    set_engine("rpart") %>%
    set_mode("classification")
  
  df_x_wf <- workflow() %>%
    add_recipe(df_x_rec) %>%
    add_model(mod_spec)

  if (is.null(cart_grid)) {
    cart_grid <- grid_regular(
      cost_complexity(range = c(-4, -1)),
      tree_depth(range = c(2L, 6L)),
      min_n(range = c(5L, 25L)),
      levels = 4
    )
  }
  
  df_x_res <- tune_grid(
    df_x_wf,
    resamples = cvs[[dataset_idx]],
    grid = cart_grid,
    metrics = metric_set(roc_auc, pr_auc, mn_log_loss),
    control = control_grid(save_pred = TRUE)
  )
  
  metrics_all <- collect_metrics(df_x_res, summarize = TRUE)
  
  best_row <- metrics_all %>%
    filter(.metric == "mn_log_loss") %>%
    arrange(mean, std_err) %>%
    head(1)
  
  best_config <- best_row$.config
  best_params <- best_row %>% select(cost_complexity, tree_depth, min_n)
  
  df_x_final_wf <- finalize_workflow(df_x_wf, best_params)
  df_x_final_fit <- workflows::fit(df_x_final_wf, data = df_x)
  
  cart_fit <- extract_fit_engine(df_x_final_fit)
  
  vip_metric_fn <- switch(
    vi_metric,
    roc_auc = function(truth, estimate) {
      yardstick::roc_auc_vec(truth = truth, estimate = estimate, event_level = "second")
    },
    pr_auc = function(truth, estimate) {
      yardstick::pr_auc_vec(truth = truth, estimate = estimate, event_level = "second")
    }
  )
  
  df_x_varimp <- vip::vi_permute(
    object = df_x_final_fit,
    feature_names = setdiff(names(df_x), SELECTED_OUTCOME_VAR),
    train = df_x,
    target = SELECTED_OUTCOME_VAR,
    metric = vip_metric_fn,
    pred_wrapper = function(object, newdata) {
      predict(object, new_data = newdata, type = "prob")$.pred_yes
    },
    nsim = vi_nsim,
    smaller_is_better = FALSE
  ) %>%
    arrange(desc(Importance))
  
  test_idx <- sample(setdiff(seq_along(datasets), dataset_idx), 1)
  df_test <- datasets[[test_idx]]
  
  df_test_preds <- predict(df_x_final_fit, new_data = df_test, type = "prob") %>%
    bind_cols(df_test %>% select(.data[[SELECTED_OUTCOME_VAR]]))
  
  test_roc_auc <- roc_auc(
    df_test_preds,
    .data[[SELECTED_OUTCOME_VAR]],
    .pred_yes,
    event_level = "second"
  )$.estimate
  
  test_pr_auc <- pr_auc(
    df_test_preds,
    .data[[SELECTED_OUTCOME_VAR]],
    .pred_yes,
    event_level = "second"
  )$.estimate
  
  test_log_loss <- mn_log_loss(
    df_test_preds,
    .data[[SELECTED_OUTCOME_VAR]],
    .pred_yes,
    event_level = "second"
  )$.estimate
  
  head_obj <- list(
    model = df_x_final_fit,
    cart_tree = cart_fit
  )
  
  list(
    dataset_key = dataset_key,
    dataset_idx = dataset_idx,
    best_params = best_params,
    best_config = best_config,
    test_roc_auc = test_roc_auc,
    test_pr_auc = test_pr_auc,
    test_log_loss = test_log_loss,
    head = head_obj,
    varimp = df_x_varimp,
    varimp_metric = vi_metric,
    varimp_nsim = vi_nsim,
    tuning = df_x_res
  )
}

dataset_keys <- names(datasets_120)[81:120]

results_81_120 <- purrr::set_names(
  purrr::map(dataset_keys, ~ run_cart_one(.x, datasets_120, cv_120)),
  dataset_keys
)

purrr::walk(results_81_120, ~ {
  head_name <- paste0("head_", .x$dataset_idx)
  varimp_name <- paste0("df_", .x$dataset_idx, "_varimp")
  
  assign(head_name, .x$head, envir = .GlobalEnv)
  assign(varimp_name, .x$varimp, envir = .GlobalEnv)
})

# Evaluating on train data (probability-based, no thresholds) ----
head_names <- ls(pattern = "^head_[0-9]+$")

heads <- setNames(
  mget(head_names),
  head_names
)

# Per-head probabilities only
df2_preds_all <- imap_dfc(heads, function(h, name) {
  p <- predict(h$model, new_data = df2, type = "prob")$.pred_yes
  tibble(!!paste0("p_", name) := p)
}) %>%
  bind_cols(df2 %>% select(.data[[SELECTED_OUTCOME_VAR]]))

# Ensemble probability summaries
df2_probs <- df2_preds_all %>%
  mutate(
    p_mean = rowMeans(across(starts_with("p_")), na.rm = TRUE),
    p_median = apply(select(., starts_with("p_")), 1, median, na.rm = TRUE),
    p_min = apply(select(., starts_with("p_")), 1, min, na.rm = TRUE),
    p_max = apply(select(., starts_with("p_")), 1, max, na.rm = TRUE)
  )

df2_votes_unajd <- df2_preds_all %>%
  mutate(
    n_yes = rowSums(across(starts_with("p_"), ~ .x >= 0.5), na.rm = TRUE),
    n_no = rowSums(across(starts_with("p_"), ~ .x < 0.5), na.rm = TRUE),
    vote_class = factor(
      if_else(n_yes > n_no, "yes", "no"),
      levels = c("no", "yes")
    )
  )

vote_conf_mat_unajd <- conf_mat(
  df2_votes_unajd,
  truth = .data[[SELECTED_OUTCOME_VAR]],
  estimate = vote_class
)

cm_unadj <- vote_conf_mat_unajd$table

tn <- cm_unadj["no", "no"]
fp <- cm_unadj["yes", "no"]
fn <- cm_unadj["no", "yes"]
tp <- cm_unadj["yes", "yes"]

sens <- tp / (tp + fn)
spec <- tn / (tn + fp)
ppv <- tp / (tp + fp)
npv <- tn / (tn + fn)

cm_metrics <- c(
  sensitivity = sens,
  specificity = spec,
  ppv = ppv,
  npv = npv
)

training_data_mean_pred_perf <- df2_probs %>%
  group_by(.data[[SELECTED_OUTCOME_VAR]]) %>%
  summarise(
    mean_p_mean = mean(p_mean, na.rm = TRUE),
    sd_p_mean = sd(p_mean, na.rm = TRUE),
    n = sum(!is.na(p_mean)),
    se = sd_p_mean / sqrt(n),
    ci_low = mean_p_mean - 1.96 * se,
    ci_high = mean_p_mean + 1.96 * se,
    .groups = "drop"
  )

ggplot(df2_probs, aes(x = p_mean, fill = as.factor(.data[[SELECTED_OUTCOME_VAR]]))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Mean Prediction by Treatment",
    subtitle = "Full training data",
    x = "Mean Probability Across Heads",
    fill = "Treatment Recieved"
  ) +
  theme_minimal()

cm_unadj
cm_metrics
training_data_mean_pred_perf

# saving everything ----
save_dir <- PATHS$ensemble_workspace

ensemble_meta <- list(
  n_heads = length(heads),
  head_names = names(heads),
  vote_rule = "pct_yes >= majority vote",
  threshold_per_head = 0.5
)

saveRDS(
  ensemble_meta,
  file = file.path(save_dir, paste0(SELECTED_OUTCOME_VAR, "_ensemble_meta.rds"))
)

purrr::iwalk(
  heads,
  ~ saveRDS(
    .x,
    file.path(save_dir, paste0(.y, ".rds"))
  )
)

performance <- list(
  training_data_mean_pred_perf = training_data_mean_pred_perf,
  training_data_conf_matrix = cm_unadj,
  training_data_metrics = cm_metrics
)

varimp_names <- ls(pattern = "^df_[0-9]+_varimp$")

varimp_list <- setNames(
  mget(varimp_names),
  varimp_names
)

saveRDS(
  performance,
  file = file.path(save_dir, paste0(SELECTED_OUTCOME_VAR, "_performance.rds"))
)

saveRDS(varimp_list, 
        file.path(save_dir, paste0(SELECTED_OUTCOME_VAR, "_var_imp.rds")))