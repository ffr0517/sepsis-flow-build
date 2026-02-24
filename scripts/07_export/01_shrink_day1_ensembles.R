# Functions ----
source(here::here("R", "bootstrap.R"))
library(rlang)

predict_ensemble_all_heads <- function(new_data, ensemble_bundle) {
  b1 <- ensemble_bundle$bundles$type1
  b2 <- ensemble_bundle$bundles$type2
  b3 <- ensemble_bundle$bundles$type3
  
  n <- nrow(new_data)
  out <- matrix(NA_real_, nrow = n, ncol = 0)
  
  # Type 1 heads
  p1 <- sapply(b1$models, function(m) {
    predict_type1_manual(new_data = new_data, preprocess = b1$preprocess, model_minimal = m)
  })
  out <- cbind(out, p1)
  
  # Type 2 heads
  p2 <- sapply(b2$models, function(m) {
    predict_type2_manual(new_data = new_data, preprocess = b2$preprocess, model_minimal = m)
  })
  out <- cbind(out, p2)
  
  # Type 3 heads
  p3 <- sapply(b3$models, function(m) {
    predict_type3_manual(new_data = new_data, preprocess = b3$preprocess, model_minimal = m)
  })
  out <- cbind(out, p3)
  
  colnames(out) <- c(
    paste0("head_", b1$head_indices),
    paste0("head_", b2$head_indices),
    paste0("head_", b3$head_indices)
  )
  
  out
}

extract_preprocess_constants_type1 <- function(head_path) {
  head <- readRDS(head_path)
  
  mold <- workflows::extract_mold(head$model)
  rec <- mold$blueprint$recipe
  
  if (is.null(rec$steps) || length(rec$steps) == 0) {
    stop("Recipe steps not found. Did you extract the right blueprint$recipe?")
  }
  
  step_med <- rec$steps[[1]]
  medians <- step_med$medians
  
  step_norm <- rec$steps[[6]]
  means <- step_norm$means
  sds <- step_norm$sds
  
  list(
    medians = medians,
    means = means,
    sds = sds
  )
}

extract_preprocess_constants_type2 <- function(head_path) {
  head <- readRDS(head_path)
  
  mold <- workflows::extract_mold(head$model)
  rec <- mold$blueprint$recipe
  
  medians <- rec$steps[[1]]$medians
  
  # Type 2 has step_normalize at the end (you saw it as step 13)
  step_norm <- rec$steps[[length(rec$steps)]]
  means <- step_norm$means
  sds <- step_norm$sds
  
  # Model feature names (truth source for what we must create)
  model_min <- strip_model_type1_glmnetpredict(head_path)
  features <- model_min$features
  
  # Pull all step_ns blocks (by class)
  ns_steps <- Filter(function(st) inherits(st, "step_ns"), rec$steps)
  if (length(ns_steps) == 0) stop("No step_ns found in recipe; not Type 2?")
  
  ns_specs <- list()
  for (st in ns_steps) {
    objs <- st$objects
    if (is.null(objs) || length(objs) == 0) stop("Found step_ns but no trained objects; recipe not trained?")
    
    for (var in names(objs)) {
      obj <- objs[[var]]
      at <- attributes(obj)
      
      expected_cols <- grep(paste0("^", var, "_ns_"), features, value = TRUE)
      if (length(expected_cols) == 0) stop("No expected spline columns found in model for var: ", var)
      
      ns_specs[[var]] <- list(
        var = var,
        knots = at$knots,
        boundary_knots = at$Boundary.knots,
        intercept = isTRUE(at$intercept),
        df = at$df,
        expected_cols = expected_cols
      )
    }
  }
  
  list(
    medians = medians,
    means = means,
    sds = sds,
    ns_specs = ns_specs,
    features = features
  )
}

extract_preprocess_constants_type3 <- function(head_path) {
  head <- readRDS(head_path)
  mold <- workflows::extract_mold(head$model)
  rec <- mold$blueprint$recipe
  
  step_med <- rec$steps[[1]]
  medians <- step_med$medians
  
  step_norm <- rec$steps[[length(rec$steps)]]
  means <- step_norm$means
  sds <- step_norm$sds
  
  list(
    medians = medians,
    means = means,
    sds = sds,
    features = names(means)
  )
}

strip_model_type1_glmnetpredict <- function(head_path) {
  head <- readRDS(head_path)
  
  penalty <- rlang::eval_tidy(head$model$fit$actions$model$spec$args$penalty)
  
  glmnet_fit <- head$model$fit$fit$fit
  lambda <- glmnet_fit$lambda
  
  lam_min <- min(lambda)
  lam_max <- max(lambda)
  
  clipped <- FALSE
  penalty_used <- penalty
  
  if (penalty > lam_max) {
    penalty_used <- lam_max
    clipped <- TRUE
  }
  if (penalty < lam_min) {
    penalty_used <- lam_min
    clipped <- TRUE
  }
  
  # IMPORTANT: use stats::predict (generic), not glmnet::predict
  coef_mat <- as.matrix(stats::predict(glmnet_fit, s = penalty_used, type = "coefficients"))
  
  intercept <- as.numeric(coef_mat["(Intercept)", 1])
  features <- setdiff(rownames(coef_mat), "(Intercept)")
  coef_vec <- as.numeric(coef_mat[features, 1])
  
  list(
    intercept = intercept,
    coefficients = coef_vec,
    features = features,
    penalty = penalty,
    penalty_used = penalty_used,
    penalty_clipped = clipped,
    lambda_min = lam_min,
    lambda_max = lam_max
  )
}

strip_model_type2_glmnetpredict <- function(head_path) {
  head <- readRDS(head_path)
  
  penalty <- rlang::eval_tidy(head$model$fit$actions$model$spec$args$penalty)
  
  glmnet_fit <- head$model$fit$fit$fit
  lambda <- glmnet_fit$lambda
  
  lam_min <- min(lambda)
  lam_max <- max(lambda)
  
  clipped <- FALSE
  penalty_used <- penalty
  if (penalty > lam_max) { penalty_used <- lam_max; clipped <- TRUE }
  if (penalty < lam_min) { penalty_used <- lam_min; clipped <- TRUE }
  
  coef_mat <- as.matrix(stats::predict(glmnet_fit, s = penalty_used, type = "coefficients"))
  
  intercept <- as.numeric(coef_mat["(Intercept)", 1])
  features <- setdiff(rownames(coef_mat), "(Intercept)")
  coef_vec <- as.numeric(coef_mat[features, 1])
  
  list(
    intercept = intercept,
    coefficients = coef_vec,
    features = features,
    penalty = penalty,
    penalty_used = penalty_used,
    penalty_clipped = clipped,
    lambda_min = lam_min,
    lambda_max = lam_max
  )
}

strip_model_type3_rpart <- function(head_path) {
  head <- readRDS(head_path)
  parsnip_fit <- head$model$fit$fit
  rpart_fit <- parsnip_fit$fit
  list(fit = rpart_fit)
}

manual_type3_predictors_df <- function(new_data, preprocess) {
  df <- as.data.frame(new_data)
  
  base_vars <- c(
    "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
    "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
  )
  
  missing_base <- setdiff(base_vars, names(df))
  if (length(missing_base) > 0) {
    stop("Missing required columns in new_data: ", paste(missing_base, collapse = ", "))
  }
  
  for (v in base_vars) {
    df[[v]] <- as.numeric(as.character(df[[v]]))
  }
  
  for (v in base_vars) {
    med <- preprocess$medians[[v]]
    if (is.null(med) || length(med) != 1) stop("Median not found for variable: ", v)
    na_idx <- is.na(df[[v]])
    if (any(na_idx)) df[[v]][na_idx] <- med
  }
  
  df[["oxy.ra"]] <- log(df[["oxy.ra"]] + 1e-06)
  df[["age_x_rr"]] <- df[["age.months"]] * df[["rr.all"]]
  df[["age_x_hr"]] <- df[["age.months"]] * df[["hr.all"]]
  
  feature_names <- preprocess$features
  missing_feats <- setdiff(feature_names, names(df))
  if (length(missing_feats) > 0) {
    stop("Missing engineered/expected features: ", paste(missing_feats, collapse = ", "))
  }
  
  Xdf <- df[, feature_names, drop = FALSE]
  
  for (v in feature_names) {
    mu <- preprocess$means[[v]]
    sd <- preprocess$sds[[v]]
    if (is.null(mu) || is.null(sd)) stop("Mean/SD not found for feature: ", v)
    Xdf[[v]] <- (Xdf[[v]] - mu) / sd
  }
  
  Xdf
}

manual_type1_matrix <- function(new_data, preprocess, feature_names) {
  df <- as.data.frame(new_data)
  
  base_vars <- c(
    "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
    "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
  )
  
  missing_base <- setdiff(base_vars, names(df))
  if (length(missing_base) > 0) {
    stop("Missing required columns in new_data: ", paste(missing_base, collapse = ", "))
  }
  
  for (v in base_vars) {
    df[[v]] <- as.numeric(as.character(df[[v]]))
  }
  
  for (v in base_vars) {
    med <- preprocess$medians[[v]]
    if (is.null(med) || length(med) != 1) stop("Median not found for variable: ", v)
    na_idx <- is.na(df[[v]])
    if (any(na_idx)) df[[v]][na_idx] <- med
  }
  
  df[["oxy.ra"]] <- log(df[["oxy.ra"]] + 1e-06)
  df[["age_x_rr"]] <- df[["age.months"]] * df[["rr.all"]]
  df[["age_x_hr"]] <- df[["age.months"]] * df[["hr.all"]]
  
  Xdf <- df[, feature_names, drop = FALSE]
  
  for (v in feature_names) {
    mu <- preprocess$means[[v]]
    sd <- preprocess$sds[[v]]
    if (is.null(mu) || is.null(sd)) stop("Mean/SD not found for feature: ", v)
    Xdf[[v]] <- (Xdf[[v]] - mu) / sd
  }
  
  as.matrix(Xdf)
}

manual_type2_matrix <- function(new_data, preprocess, feature_names = preprocess$features) {
  df <- as.data.frame(new_data)
  
  base_vars <- c(
    "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
    "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
  )
  
  missing_base <- setdiff(base_vars, names(df))
  if (length(missing_base) > 0) {
    stop("Missing required columns in new_data: ", paste(missing_base, collapse = ", "))
  }
  
  for (v in base_vars) df[[v]] <- as.numeric(as.character(df[[v]]))
  
  for (v in base_vars) {
    med <- preprocess$medians[[v]]
    if (is.null(med) || length(med) != 1) stop("Median not found for variable: ", v)
    na_idx <- is.na(df[[v]])
    if (any(na_idx)) df[[v]][na_idx] <- med
  }
  
  # step_mutate
  df[["oxy.ra"]] <- log(df[["oxy.ra"]] + 1e-06)
  df[["age_x_rr"]] <- df[["age.months"]] * df[["rr.all"]]
  df[["age_x_hr"]] <- df[["age.months"]] * df[["hr.all"]]
  
  # step_ns expansions
  for (var in names(preprocess$ns_specs)) {
    spec <- preprocess$ns_specs[[var]]
    
    basis <- splines::ns(
      df[[var]],
      knots = spec$knots,
      Boundary.knots = spec$boundary_knots,
      intercept = isTRUE(spec$intercept),
      df = spec$df
    )
    
    if (ncol(basis) != length(spec$expected_cols)) {
      stop("Spline column count mismatch for ", var,
           ": basis=", ncol(basis), " expected=", length(spec$expected_cols))
    }
    
    colnames(basis) <- spec$expected_cols
    for (j in seq_len(ncol(basis))) {
      df[[colnames(basis)[j]]] <- basis[, j]
    }
    
    if (!(var %in% feature_names) && (var %in% names(df))) {
      df[[var]] <- NULL
    }
  }
  
  # subset in exact model order
  Xdf <- df[, feature_names, drop = FALSE]
  
  # normalize
  for (v in feature_names) {
    mu <- preprocess$means[[v]]
    sd <- preprocess$sds[[v]]
    if (is.null(mu) || is.null(sd)) stop("Mean/SD not found for feature: ", v)
    Xdf[[v]] <- (Xdf[[v]] - mu) / sd
  }
  
  as.matrix(Xdf)
}

predict_type1_minimal_from_X <- function(X, model_minimal) {
  eta <- as.numeric(model_minimal$intercept) + as.vector(X %*% model_minimal$coefficients)
  1 / (1 + exp(-eta))
}

predict_type1_manual <- function(new_data, preprocess, model_minimal) {
  X <- manual_type1_matrix(new_data, preprocess, model_minimal$features)
  predict_type1_minimal_from_X(X, model_minimal)
}

predict_type2_manual <- function(new_data, preprocess, model_minimal) {
  X <- manual_type2_matrix(new_data, preprocess, model_minimal$features)
  eta <- as.numeric(model_minimal$intercept) + as.vector(X %*% model_minimal$coefficients)
  1 / (1 + exp(-eta))
}

predict_type3_manual <- function(new_data, preprocess, model_minimal) {
  Xdf <- manual_type3_matrix(new_data, preprocess)
  
  p <- stats::predict(model_minimal$fit, newdata = Xdf, type = "prob")
  if (!is.matrix(p) || !"yes" %in% colnames(p)) {
    stop("Unexpected rpart probability output; expected a matrix with column 'yes'.")
  }
  as.numeric(p[, "yes"])
}

predict_minimal <- function(new_data, preprocess, model_minimal) {
  X <- manual_type2_matrix(new_data, preprocess)
  eta <- model_minimal$intercept + as.vector(X %*% model_minimal$coefficients)
  1 / (1 + exp(-eta))
}

extract_preprocess_constants_type3 <- function(head_path) {
  head <- readRDS(head_path)
  
  mold <- workflows::extract_mold(head$model)
  rec <- mold$blueprint$recipe
  
  if (is.null(rec$steps) || length(rec$steps) == 0) {
    stop("Recipe steps not found. Did you extract the right blueprint$recipe?")
  }
  
  step_med <- rec$steps[[1]]
  medians <- step_med$medians
  
  step_norm <- rec$steps[[6]]
  means <- step_norm$means
  sds <- step_norm$sds
  
  list(
    medians = medians,
    means = means,
    sds = sds
  )
}

strip_model_type3_rpart <- function(head_path) {
  head <- readRDS(head_path)
  
  # decision_tree() -> rpart engine by default here
  # keep only what we need for prediction: the fitted rpart object
  rpart_fit <- head$model$fit$fit$fit
  if (!inherits(rpart_fit, "rpart")) {
    stop("Expected an rpart fit, got: ", paste(class(rpart_fit), collapse = ", "))
  }
  
  list(
    engine = "rpart",
    fit = rpart_fit
  )
}

manual_type3_matrix <- function(new_data, preprocess) {
  df <- as.data.frame(new_data)
  
  base_vars <- c(
    "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
    "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
  )
  
  missing_base <- setdiff(base_vars, names(df))
  if (length(missing_base) > 0) {
    stop("Missing required columns in new_data: ", paste(missing_base, collapse = ", "))
  }
  
  for (v in base_vars) {
    df[[v]] <- as.numeric(as.character(df[[v]]))
  }
  
  for (v in base_vars) {
    med <- preprocess$medians[[v]]
    if (is.null(med) || length(med) != 1) stop("Median not found for variable: ", v)
    na_idx <- is.na(df[[v]])
    if (any(na_idx)) df[[v]][na_idx] <- med
  }
  
  df[["oxy.ra"]] <- log(df[["oxy.ra"]] + 1e-06)
  df[["age_x_rr"]] <- df[["age.months"]] * df[["rr.all"]]
  df[["age_x_hr"]] <- df[["age.months"]] * df[["hr.all"]]
  
  feature_names <- c(
    "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
    "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long",
    "oxy.ra", "age_x_rr", "age_x_hr"
  )
  
  Xdf <- df[, feature_names, drop = FALSE]
  
  for (v in feature_names) {
    mu <- preprocess$means[[v]]
    sd <- preprocess$sds[[v]]
    if (is.null(mu) || is.null(sd)) stop("Mean/SD not found for feature: ", v)
    Xdf[[v]] <- (Xdf[[v]] - mu) / sd
  }
  
  Xdf
}

predict_type3_manual <- function(new_data, preprocess, model_minimal) {
  Xdf <- manual_type3_matrix(new_data, preprocess)
  
  # IMPORTANT: rpart uses variable names in the tree; it expects a data.frame
  # with those exact columns.
  p <- stats::predict(model_minimal$fit, newdata = Xdf, type = "prob")
  if (!is.matrix(p) || !"yes" %in% colnames(p)) {
    stop("Unexpected rpart probability output; expected a matrix with column 'yes'.")
  }
  as.numeric(p[, "yes"])
}

# Loading in test data ----
test_data <- readRDS(file.path(PATHS$processed, "test.rds"))
test_subset <- test_data[1:50, ]

# --- Level 5 (non-bolused IV fluids) ----
# Type 1 
build_type1_bundle_1to40 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L5", "models"),
    head_indices = 1:40,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  preprocess <- extract_preprocess_constants_type1(head_paths[[1]])
  
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type1_glmnetpredict(head_paths[[i]])
  }
  
  ref_features <- models[[1]]$features
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$features, ref_features)) {
      stop("Feature mismatch detected at head index ", head_indices[[i]], ".")
    }
  }
  
  clipped_idx <- head_indices[vapply(models, function(m) isTRUE(m$penalty_clipped), logical(1))]
  if (length(clipped_idx) > 0) {
    message(
      "Note: penalty was outside lambda range and got clipped for heads: ",
      paste(clipped_idx, collapse = ", ")
    )
  }
  
  list(
    type = "type1",
    head_indices = head_indices,
    preprocess = preprocess,
    features = ref_features,
    models = models
  )
}

bundle_type1_heads_1to40 <- build_type1_bundle_1to40(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L5", "models"),
  head_indices = 1:40
)

# Type 2 
build_type2_bundle_41to80 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L5", "models"),
    head_indices = 41:80,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  # Preprocess constants are shared across all type-2 heads, so extract once
  preprocess <- extract_preprocess_constants_type2(head_paths[[1]])
  
  # Strip each head down to intercept + coefficients (at penalty_used) + feature names
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type2_glmnetpredict(head_paths[[i]])
  }
  
  # Sanity: all heads must have identical feature ordering
  ref_features <- models[[1]]$features
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$features, ref_features)) {
      stop("Feature mismatch detected at head index ", head_indices[[i]], ".")
    }
  }
  
  clipped_idx <- head_indices[vapply(models, function(m) isTRUE(m$penalty_clipped), logical(1))]
  if (length(clipped_idx) > 0) {
    message(
      "Note: penalty was outside lambda range and got clipped for heads: ",
      paste(clipped_idx, collapse = ", ")
    )
  }
  
  list(
    type = "type2",
    head_indices = head_indices,
    preprocess = preprocess,
    features = ref_features,
    models = models
  )
}

bundle_type2_heads_41to80 <- build_type2_bundle_41to80(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L5", "models"),
  head_indices = 41:80
)

cat("bundle_type2_heads_41to80 object.size (MB):",
    as.numeric(object.size(bundle_type2_heads_41to80)) / 1024^2, "\n")

tmp_bundle_path <- tempfile(fileext = ".rds")
saveRDS(bundle_type2_heads_41to80, tmp_bundle_path, compress = "xz")
cat("bundle_type2_heads_41to80 saved (xz) MB:",
    file.info(tmp_bundle_path)$size / 1024^2, "\n")
unlink(tmp_bundle_path)

# Type 3 
library(workflows)

extract_preprocess_constants_type3 <- function(head_path) {
  head <- readRDS(head_path)
  
  mold <- workflows::extract_mold(head$model)
  rec <- mold$blueprint$recipe
  
  if (is.null(rec$steps) || length(rec$steps) == 0) {
    stop("Recipe steps not found. Did you extract the right blueprint$recipe?")
  }
  
  step_med <- rec$steps[[1]]
  medians <- step_med$medians
  
  step_norm <- rec$steps[[6]]
  means <- step_norm$means
  sds <- step_norm$sds
  
  list(
    medians = medians,
    means = means,
    sds = sds
  )
}

strip_model_type3_rpart <- function(head_path) {
  head <- readRDS(head_path)
  
  # decision_tree() -> rpart engine by default here
  # keep only what we need for prediction: the fitted rpart object
  rpart_fit <- head$model$fit$fit$fit
  if (!inherits(rpart_fit, "rpart")) {
    stop("Expected an rpart fit, got: ", paste(class(rpart_fit), collapse = ", "))
  }
  
  list(
    engine = "rpart",
    fit = rpart_fit
  )
}

manual_type3_matrix <- function(new_data, preprocess) {
  df <- as.data.frame(new_data)
  
  base_vars <- c(
    "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
    "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
  )
  
  missing_base <- setdiff(base_vars, names(df))
  if (length(missing_base) > 0) {
    stop("Missing required columns in new_data: ", paste(missing_base, collapse = ", "))
  }
  
  for (v in base_vars) {
    df[[v]] <- as.numeric(as.character(df[[v]]))
  }
  
  for (v in base_vars) {
    med <- preprocess$medians[[v]]
    if (is.null(med) || length(med) != 1) stop("Median not found for variable: ", v)
    na_idx <- is.na(df[[v]])
    if (any(na_idx)) df[[v]][na_idx] <- med
  }
  
  df[["oxy.ra"]] <- log(df[["oxy.ra"]] + 1e-06)
  df[["age_x_rr"]] <- df[["age.months"]] * df[["rr.all"]]
  df[["age_x_hr"]] <- df[["age.months"]] * df[["hr.all"]]
  
  feature_names <- c(
    "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
    "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long",
    "oxy.ra", "age_x_rr", "age_x_hr"
  )
  
  Xdf <- df[, feature_names, drop = FALSE]
  
  for (v in feature_names) {
    mu <- preprocess$means[[v]]
    sd <- preprocess$sds[[v]]
    if (is.null(mu) || is.null(sd)) stop("Mean/SD not found for feature: ", v)
    Xdf[[v]] <- (Xdf[[v]] - mu) / sd
  }
  
  Xdf
}

predict_type3_manual <- function(new_data, preprocess, model_minimal) {
  Xdf <- manual_type3_matrix(new_data, preprocess)
  
  # IMPORTANT: rpart uses variable names in the tree; it expects a data.frame
  # with those exact columns.
  p <- stats::predict(model_minimal$fit, newdata = Xdf, type = "prob")
  if (!is.matrix(p) || !"yes" %in% colnames(p)) {
    stop("Unexpected rpart probability output; expected a matrix with column 'yes'.")
  }
  as.numeric(p[, "yes"])
}

build_type3_bundle_81to120 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L5", "models"),
    head_indices = 81:120,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  preprocess <- extract_preprocess_constants_type3(head_paths[[1]])
  
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type3_rpart(head_paths[[i]])
  }
  
  list(
    type = "type3",
    head_indices = head_indices,
    preprocess = preprocess,
    models = models
  )
}

# Build + size audit 
bundle_type3_heads_81to120 <- build_type3_bundle_81to120(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L5", "models"),
  head_indices = 81:120
)

cat("bundle_type3_heads_81to120 object.size (MB):",
    as.numeric(object.size(bundle_type3_heads_81to120)) / 1024^2, "\n")

tmp_bundle_path <- tempfile(fileext = ".rds")
saveRDS(bundle_type3_heads_81to120, tmp_bundle_path, compress = "xz")
cat("bundle_type3_heads_81to120 saved (xz) MB:",
    file.info(tmp_bundle_path)$size / 1024^2, "\n")
unlink(tmp_bundle_path)

# Joining bundles 
bundle_ensemble1 <- list(
  ensemble = "ensemble1",
  created_utc = format(Sys.time(), tz = "UTC"),
  bundles = list(
    type1 = bundle_type1_heads_1to40,
    type2 = bundle_type2_heads_41to80,
    type3 = bundle_type3_heads_81to120
  )
)

# Saving joint bundle 
ensemble1_path <- "sepsis-flow-D1-L5/models/ensemble1_bundle.rds"

saveRDS(bundle_ensemble1, ensemble1_path, compress = "xz")

# Re-loading bundle for test 
bundle_ensemble1_reloaded <- readRDS(ensemble1_path)

cat("Saved bundle size (xz) MB:",
    file.info(ensemble1_path)$size / 1024^2, "\n")

predict_ensemble1_summary <- function(new_data, ensemble_bundle, vote_threshold = 0.5) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)
  
  mean_prob <- rowMeans(pmat, na.rm = TRUE)
  n_votes <- rowSums(pmat > vote_threshold, na.rm = TRUE)
  
  n_heads <- rowSums(!is.na(pmat))
  vote_frac <- ifelse(n_heads > 0, n_votes / n_heads, NA_real_)
  
  data.frame(
    mean_prob = mean_prob,
    n_votes_gt = n_votes,
    n_heads = n_heads,
    vote_frac_gt = vote_frac
  )
}

predict_ensemble1_mean <- function(new_data, ensemble_bundle) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)
  rowMeans(pmat, na.rm = TRUE)
}

summary_df <- predict_ensemble1_summary(test_subset, bundle_ensemble1_reloaded, vote_threshold = 0.5)

summary_df$ensemble_yes <- summary_df$n_votes_gt > (summary_df$n_heads / 2)

head(summary_df, 10)


# --- Level 4 (O2 via face or nasal cannula) ----
# Type 1 
build_type1_bundle_1to40 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L4", "models"),
    head_indices = 1:40,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  preprocess <- extract_preprocess_constants_type1(head_paths[[1]])
  
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type1_glmnetpredict(head_paths[[i]])
  }
  
  ref_features <- models[[1]]$features
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$features, ref_features)) {
      stop("Feature mismatch detected at head index ", head_indices[[i]], ".")
    }
  }
  
  clipped_idx <- head_indices[vapply(models, function(m) isTRUE(m$penalty_clipped), logical(1))]
  if (length(clipped_idx) > 0) {
    message(
      "Note: penalty was outside lambda range and got clipped for heads: ",
      paste(clipped_idx, collapse = ", ")
    )
  }
  
  list(
    type = "type1",
    head_indices = head_indices,
    preprocess = preprocess,
    features = ref_features,
    models = models
  )
}

bundle_type1_heads_1to40 <- build_type1_bundle_1to40(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L4", "models"),
  head_indices = 1:40
)

# Type 2 
build_type2_bundle_41to80 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L4", "models"),
    head_indices = 41:80,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  # Preprocess constants are shared across all type-2 heads, so extract once
  preprocess <- extract_preprocess_constants_type2(head_paths[[1]])
  
  # Strip each head down to intercept + coefficients (at penalty_used) + feature names
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type2_glmnetpredict(head_paths[[i]])
  }
  
  # Sanity: all heads must have identical feature ordering
  ref_features <- models[[1]]$features
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$features, ref_features)) {
      stop("Feature mismatch detected at head index ", head_indices[[i]], ".")
    }
  }
  
  clipped_idx <- head_indices[vapply(models, function(m) isTRUE(m$penalty_clipped), logical(1))]
  if (length(clipped_idx) > 0) {
    message(
      "Note: penalty was outside lambda range and got clipped for heads: ",
      paste(clipped_idx, collapse = ", ")
    )
  }
  
  list(
    type = "type2",
    head_indices = head_indices,
    preprocess = preprocess,
    features = ref_features,
    models = models
  )
}

bundle_type2_heads_41to80 <- build_type2_bundle_41to80(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L4", "models"),
  head_indices = 41:80
)

# Type 3 
build_type3_bundle_81to120 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L4", "models"),
    head_indices = 81:120,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  preprocess <- extract_preprocess_constants_type3(head_paths[[1]])
  
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type3_rpart(head_paths[[i]])
  }
  
  list(
    type = "type3",
    head_indices = head_indices,
    preprocess = preprocess,
    models = models
  )
}

# Build + size audit 
bundle_type3_heads_81to120 <- build_type3_bundle_81to120(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L4", "models"),
  head_indices = 81:120
)

# Joining bundles 
bundle_ensemble2 <- list(
  ensemble = "ensemble2",
  created_utc = format(Sys.time(), tz = "UTC"),
  bundles = list(
    type1 = bundle_type1_heads_1to40,
    type2 = bundle_type2_heads_41to80,
    type3 = bundle_type3_heads_81to120
  )
)

# Saving joint bundle 
ensemble2_path <- "sepsis-flow-D1-L4/models/ensemble2_bundle.rds"

saveRDS(bundle_ensemble2, ensemble2_path, compress = "xz")

# Re-loading bundle for test 
bundle_ensemble2_reloaded <- readRDS(ensemble2_path)

cat("Saved bundle size (xz) MB:",
    file.info(ensemble2_path)$size / 1024^2, "\n")

predict_ensemble2_summary <- function(new_data, ensemble_bundle, vote_threshold = 0.5) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)
  
  mean_prob <- rowMeans(pmat, na.rm = TRUE)
  n_votes <- rowSums(pmat > vote_threshold, na.rm = TRUE)
  
  n_heads <- rowSums(!is.na(pmat))
  vote_frac <- ifelse(n_heads > 0, n_votes / n_heads, NA_real_)
  
  data.frame(
    mean_prob = mean_prob,
    n_votes_gt = n_votes,
    n_heads = n_heads,
    vote_frac_gt = vote_frac
  )
}

predict_ensemble2_mean <- function(new_data, ensemble_bundle) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)
  rowMeans(pmat, na.rm = TRUE)
}

summary_df <- predict_ensemble2_summary(test_subset, bundle_ensemble2_reloaded, vote_threshold = 0.5)

summary_df$ensemble_yes <- summary_df$n_votes_gt > (summary_df$n_heads / 2)

head(summary_df, 10)


# --- Level 3 (ICU admission with clinical reason) ----
# Type 1 
build_type1_bundle_1to40 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L3", "models"),
    head_indices = 1:40,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  preprocess <- extract_preprocess_constants_type1(head_paths[[1]])
  
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type1_glmnetpredict(head_paths[[i]])
  }
  
  ref_features <- models[[1]]$features
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$features, ref_features)) {
      stop("Feature mismatch detected at head index ", head_indices[[i]], ".")
    }
  }
  
  clipped_idx <- head_indices[vapply(models, function(m) isTRUE(m$penalty_clipped), logical(1))]
  if (length(clipped_idx) > 0) {
    message(
      "Note: penalty was outside lambda range and got clipped for heads: ",
      paste(clipped_idx, collapse = ", ")
    )
  }
  
  list(
    type = "type1",
    head_indices = head_indices,
    preprocess = preprocess,
    features = ref_features,
    models = models
  )
}

bundle_type1_heads_1to40 <- build_type1_bundle_1to40(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L3", "models"),
  head_indices = 1:40
)

# Type 2 
build_type2_bundle_41to80 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L3", "models"),
    head_indices = 41:80,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  # Preprocess constants are shared across all type-2 heads, so extract once
  preprocess <- extract_preprocess_constants_type2(head_paths[[1]])
  
  # Strip each head down to intercept + coefficients (at penalty_used) + feature names
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type2_glmnetpredict(head_paths[[i]])
  }
  
  # Sanity: all heads must have identical feature ordering
  ref_features <- models[[1]]$features
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$features, ref_features)) {
      stop("Feature mismatch detected at head index ", head_indices[[i]], ".")
    }
  }
  
  clipped_idx <- head_indices[vapply(models, function(m) isTRUE(m$penalty_clipped), logical(1))]
  if (length(clipped_idx) > 0) {
    message(
      "Note: penalty was outside lambda range and got clipped for heads: ",
      paste(clipped_idx, collapse = ", ")
    )
  }
  
  list(
    type = "type2",
    head_indices = head_indices,
    preprocess = preprocess,
    features = ref_features,
    models = models
  )
}

bundle_type2_heads_41to80 <- build_type2_bundle_41to80(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L3", "models"),
  head_indices = 41:80
)

# Type 3 
build_type3_bundle_81to120 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L3", "models"),
    head_indices = 81:120,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  preprocess <- extract_preprocess_constants_type3(head_paths[[1]])
  
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type3_rpart(head_paths[[i]])
  }
  
  list(
    type = "type3",
    head_indices = head_indices,
    preprocess = preprocess,
    models = models
  )
}

# Build + size audit 
bundle_type3_heads_81to120 <- build_type3_bundle_81to120(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L3", "models"),
  head_indices = 81:120
)

# Joining bundles 
bundle_ensemble3 <- list(
  ensemble = "ensemble3",
  created_utc = format(Sys.time(), tz = "UTC"),
  bundles = list(
    type1 = bundle_type1_heads_1to40,
    type2 = bundle_type2_heads_41to80,
    type3 = bundle_type3_heads_81to120
  )
)

# Saving joint bundle 
ensemble3_path <- "sepsis-flow-D1-L3/models/ensemble3_bundle.rds"

saveRDS(bundle_ensemble3, ensemble3_path, compress = "xz")

# Re-loading bundle for test 
bundle_ensemble3_reloaded <- readRDS(ensemble3_path)

cat("Saved bundle size (xz) MB:",
    file.info(ensemble3_path)$size / 1024^2, "\n")

predict_ensemble3_summary <- function(new_data, ensemble_bundle, vote_threshold = 0.5) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)
  
  mean_prob <- rowMeans(pmat, na.rm = TRUE)
  n_votes <- rowSums(pmat > vote_threshold, na.rm = TRUE)
  
  n_heads <- rowSums(!is.na(pmat))
  vote_frac <- ifelse(n_heads > 0, n_votes / n_heads, NA_real_)
  
  data.frame(
    mean_prob = mean_prob,
    n_votes_gt = n_votes,
    n_heads = n_heads,
    vote_frac_gt = vote_frac
  )
}

predict_ensemble3_mean <- function(new_data, ensemble_bundle) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)
  rowMeans(pmat, na.rm = TRUE)
}

summary_df <- predict_ensemble3_summary(test_subset, bundle_ensemble3_reloaded, vote_threshold = 0.5)

summary_df$ensemble_yes <- summary_df$n_votes_gt > (summary_df$n_heads / 2)

head(summary_df, 10)


# --- Level 2 (CPAP or IV fluid bolus) ----
# Type 1 
build_type1_bundle_1to40 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L2", "models"),
    head_indices = 1:40,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  preprocess <- extract_preprocess_constants_type1(head_paths[[1]])
  
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type1_glmnetpredict(head_paths[[i]])
  }
  
  ref_features <- models[[1]]$features
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$features, ref_features)) {
      stop("Feature mismatch detected at head index ", head_indices[[i]], ".")
    }
  }
  
  clipped_idx <- head_indices[vapply(models, function(m) isTRUE(m$penalty_clipped), logical(1))]
  if (length(clipped_idx) > 0) {
    message(
      "Note: penalty was outside lambda range and got clipped for heads: ",
      paste(clipped_idx, collapse = ", ")
    )
  }
  
  list(
    type = "type1",
    head_indices = head_indices,
    preprocess = preprocess,
    features = ref_features,
    models = models
  )
}

bundle_type1_heads_1to40 <- build_type1_bundle_1to40(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L2", "models"),
  head_indices = 1:40
)

# Type 2 
build_type2_bundle_41to80 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L2", "models"),
    head_indices = 41:80,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  # Preprocess constants are shared across all type-2 heads, so extract once
  preprocess <- extract_preprocess_constants_type2(head_paths[[1]])
  
  # Strip each head down to intercept + coefficients (at penalty_used) + feature names
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type2_glmnetpredict(head_paths[[i]])
  }
  
  # Sanity: all heads must have identical feature ordering
  ref_features <- models[[1]]$features
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$features, ref_features)) {
      stop("Feature mismatch detected at head index ", head_indices[[i]], ".")
    }
  }
  
  clipped_idx <- head_indices[vapply(models, function(m) isTRUE(m$penalty_clipped), logical(1))]
  if (length(clipped_idx) > 0) {
    message(
      "Note: penalty was outside lambda range and got clipped for heads: ",
      paste(clipped_idx, collapse = ", ")
    )
  }
  
  list(
    type = "type2",
    head_indices = head_indices,
    preprocess = preprocess,
    features = ref_features,
    models = models
  )
}

bundle_type2_heads_41to80 <- build_type2_bundle_41to80(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L2", "models"),
  head_indices = 41:80
)

# Type 3 
build_type3_bundle_81to120 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L2", "models"),
    head_indices = 81:120,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  preprocess <- extract_preprocess_constants_type3(head_paths[[1]])
  
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type3_rpart(head_paths[[i]])
  }
  
  list(
    type = "type3",
    head_indices = head_indices,
    preprocess = preprocess,
    models = models
  )
}

# Build + size audit 
bundle_type3_heads_81to120 <- build_type3_bundle_81to120(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L2", "models"),
  head_indices = 81:120
)

# Joining bundles 
bundle_ensemble4 <- list(
  ensemble = "ensemble4",
  created_utc = format(Sys.time(), tz = "UTC"),
  bundles = list(
    type1 = bundle_type1_heads_1to40,
    type2 = bundle_type2_heads_41to80,
    type3 = bundle_type3_heads_81to120
  )
)

# Saving joint bundle 
ensemble4_path <- "sepsis-flow-D1-L2/models/ensemble4_bundle.rds"

saveRDS(bundle_ensemble4, ensemble4_path, compress = "xz")

# Re-loading bundle for test 
bundle_ensemble4_reloaded <- readRDS(ensemble4_path)

cat("Saved bundle size (xz) MB:",
    file.info(ensemble4_path)$size / 1024^2, "\n")

predict_ensemble4_summary <- function(new_data, ensemble_bundle, vote_threshold = 0.5) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)
  
  mean_prob <- rowMeans(pmat, na.rm = TRUE)
  n_votes <- rowSums(pmat > vote_threshold, na.rm = TRUE)
  
  n_heads <- rowSums(!is.na(pmat))
  vote_frac <- ifelse(n_heads > 0, n_votes / n_heads, NA_real_)
  
  data.frame(
    mean_prob = mean_prob,
    n_votes_gt = n_votes,
    n_heads = n_heads,
    vote_frac_gt = vote_frac
  )
}

predict_ensemble4_mean <- function(new_data, ensemble_bundle) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)
  rowMeans(pmat, na.rm = TRUE)
}

summary_df <- predict_ensemble4_summary(test_subset, bundle_ensemble4_reloaded, vote_threshold = 0.5)

summary_df$ensemble_yes <- summary_df$n_votes_gt > (summary_df$n_heads / 2)

head(summary_df, 10)


# --- Level 1 (mechanical ventilation, inotropes, or renal replacement therapy) ----
# Type 1 
build_type1_bundle_1to40 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L1", "models"),
    head_indices = 1:40,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  preprocess <- extract_preprocess_constants_type1(head_paths[[1]])
  
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type1_glmnetpredict(head_paths[[i]])
  }
  
  ref_features <- models[[1]]$features
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$features, ref_features)) {
      stop("Feature mismatch detected at head index ", head_indices[[i]], ".")
    }
  }
  
  clipped_idx <- head_indices[vapply(models, function(m) isTRUE(m$penalty_clipped), logical(1))]
  if (length(clipped_idx) > 0) {
    message(
      "Note: penalty was outside lambda range and got clipped for heads: ",
      paste(clipped_idx, collapse = ", ")
    )
  }
  
  list(
    type = "type1",
    head_indices = head_indices,
    preprocess = preprocess,
    features = ref_features,
    models = models
  )
}

bundle_type1_heads_1to40 <- build_type1_bundle_1to40(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L1", "models"),
  head_indices = 1:40
)

# Type 2 
build_type2_bundle_41to80 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L1", "models"),
    head_indices = 41:80,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  # Preprocess constants are shared across all type-2 heads, so extract once
  preprocess <- extract_preprocess_constants_type2(head_paths[[1]])
  
  # Strip each head down to intercept + coefficients (at penalty_used) + feature names
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type2_glmnetpredict(head_paths[[i]])
  }
  
  # Sanity: all heads must have identical feature ordering
  ref_features <- models[[1]]$features
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$features, ref_features)) {
      stop("Feature mismatch detected at head index ", head_indices[[i]], ".")
    }
  }
  
  clipped_idx <- head_indices[vapply(models, function(m) isTRUE(m$penalty_clipped), logical(1))]
  if (length(clipped_idx) > 0) {
    message(
      "Note: penalty was outside lambda range and got clipped for heads: ",
      paste(clipped_idx, collapse = ", ")
    )
  }
  
  list(
    type = "type2",
    head_indices = head_indices,
    preprocess = preprocess,
    features = ref_features,
    models = models
  )
}

bundle_type2_heads_41to80 <- build_type2_bundle_41to80(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L1", "models"),
  head_indices = 41:80
)

# Type 3 
build_type3_bundle_81to120 <- function(
    head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L1", "models"),
    head_indices = 81:120,
    head_prefix = "head_",
    head_suffix = ".rds"
) {
  head_paths <- file.path(head_dir, paste0(head_prefix, head_indices, head_suffix))
  
  missing_files <- head_paths[!file.exists(head_paths)]
  if (length(missing_files) > 0) {
    stop("Missing head files:\n", paste(missing_files, collapse = "\n"))
  }
  
  preprocess <- extract_preprocess_constants_type3(head_paths[[1]])
  
  models <- vector("list", length(head_paths))
  for (i in seq_along(head_paths)) {
    models[[i]] <- strip_model_type3_rpart(head_paths[[i]])
  }
  
  list(
    type = "type3",
    head_indices = head_indices,
    preprocess = preprocess,
    models = models
  )
}

# Build + size audit 
bundle_type3_heads_81to120 <- build_type3_bundle_81to120(
  head_dir = file.path(PATHS$outputs_exports, "sepsis-flow-D1-L1", "models"),
  head_indices = 81:120
)

# Joining bundles 
bundle_ensemble5 <- list(
  ensemble = "ensemble5",
  created_utc = format(Sys.time(), tz = "UTC"),
  bundles = list(
    type1 = bundle_type1_heads_1to40,
    type2 = bundle_type2_heads_41to80,
    type3 = bundle_type3_heads_81to120
  )
)

# Saving joint bundle 
ensemble5_path <- "sepsis-flow-D1-L1/models/ensemble5_bundle.rds"

saveRDS(bundle_ensemble5, ensemble5_path, compress = "xz")

# Re-loading bundle for test 
bundle_ensemble5_reloaded <- readRDS(ensemble5_path)

cat("Saved bundle size (xz) MB:",
    file.info(ensemble5_path)$size / 1024^2, "\n")

predict_ensemble5_summary <- function(new_data, ensemble_bundle, vote_threshold = 0.5) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)
  
  mean_prob <- rowMeans(pmat, na.rm = TRUE)
  n_votes <- rowSums(pmat > vote_threshold, na.rm = TRUE)
  
  n_heads <- rowSums(!is.na(pmat))
  vote_frac <- ifelse(n_heads > 0, n_votes / n_heads, NA_real_)
  
  data.frame(
    mean_prob = mean_prob,
    n_votes_gt = n_votes,
    n_heads = n_heads,
    vote_frac_gt = vote_frac
  )
}

predict_ensemble5_mean <- function(new_data, ensemble_bundle) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)
  rowMeans(pmat, na.rm = TRUE)
}

summary_df <- predict_ensemble5_summary(test_subset, bundle_ensemble5_reloaded, vote_threshold = 0.5)

summary_df$ensemble_yes <- summary_df$n_votes_gt > (summary_df$n_heads / 2)

head(summary_df, 10)

# Saving day 1 treatment bundles ----
bundle_day1 <- list(
  kind = "day1_bundle",
  created_utc = format(Sys.time(), tz = "UTC"),
  levels = list(
    L5 = bundle_ensemble1,
    L4 = bundle_ensemble2,
    L3 = bundle_ensemble3,
    L2 = bundle_ensemble4,
    L1 = bundle_ensemble5
  ),
  meta = list(
    vote_threshold_default = 0.5,
    vote_rule = "prob_gt_threshold",
    ensemble_yes_rule = "strict_majority"
  )
)

dir.create(dirname(file.path(PATHS$outputs_exports, "sepsis-flow-day1-treatments", "sepsis-flow-D1", "day1_bundle.rds")), recursive = TRUE, showWarnings = FALSE)
saveRDS(bundle_day1, file.path(PATHS$outputs_exports, "sepsis-flow-day1-treatments", "sepsis-flow-D1", "day1_bundle.rds"), compress = "xz")