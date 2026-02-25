# Variable importance JSON export ----

source(here::here("R", "bootstrap.R"))

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(purrr)
})

# Paths ----
varimp_dir <- PATHS$ensemble_workspace
json_out_dir <- PATHS$outputs_analysis
dir.create(json_out_dir, recursive = TRUE, showWarnings = FALSE)

# Loading variable importance data ----
rds_files <- list.files(varimp_dir, pattern = "_var_imp\\.rds$", full.names = TRUE)

if (!length(rds_files)) {
  stop("No *_var_imp.rds files found in: ", varimp_dir)
}

varimp_objects <- lapply(rds_files, readRDS)
varimp_names <- tools::file_path_sans_ext(basename(rds_files))
names(varimp_objects) <- varimp_names

invisible(mapply(
  assign,
  varimp_names,
  varimp_objects,
  MoreArgs = list(envir = .GlobalEnv)
))

# Helpers ----
summarise_varimp <- function(varimp_obj) {
  varimp_obj %>%
    bind_rows(.id = "head") %>%
    group_by(Variable) %>%
    summarise(
      n_heads = n(),
      mean_importance = mean(Importance, na.rm = TRUE),
      sd_across_heads = sd(Importance, na.rm = TRUE),
      se = sd_across_heads / sqrt(n_heads),
      ci_lower = mean_importance - 1.96 * se,
      ci_upper = mean_importance + 1.96 * se,
      .groups = "drop"
    ) %>%
    filter(!(mean_importance == 0 &
      sd_across_heads == 0 &
      se == 0 &
      ci_lower == 0 &
      ci_upper == 0)) %>%
    arrange(desc(mean_importance))
}

treatment_labels <- list(
  L1 = "Mechanical ventilation, inotropes, or renal replacement therapy",
  L2 = "CPAP or IV fluid bolus",
  L3 = "ICU admission",
  L4 = "O2 via face mask or nasal cannula",
  L5 = "Non-bolused IV fluids"
)

make_outcome_block <- function(df, outcome_key) {
  list(
    outcome_key = outcome_key,
    outcome_label = outcome_key,
    variables = df %>%
      transmute(
        feature_key = Variable,
        feature_label = Variable,
        mean = round(mean_importance, 4),
        ci_low = round(ci_lower, 4),
        ci_high = round(ci_upper, 4)
      ) %>%
      split(seq_len(nrow(.))) %>%
      lapply(as.list)
  )
}

build_day_json <- function(day) {
  outcomes <- map(1:5, function(level) {
    obj_name <- paste0("LEVEL", level, "_TREATMENTS_D", day, "_SAFE_0_var_imp")
    if (!exists(obj_name, inherits = FALSE)) {
      stop("Required variable importance object not found: ", obj_name)
    }

    df_summary <- summarise_varimp(get(obj_name, inherits = FALSE))
    outcome_key <- treatment_labels[[paste0("L", level)]]
    make_outcome_block(df_summary, outcome_key)
  })

  list(
    version = "v1",
    metric = "averaged_variable_importance",
    ci_level = 0.95,
    day = paste0("day", day),
    outcomes = outcomes
  )
}

# Build + save ----
day1_json <- build_day_json(1)
day2_json <- build_day_json(2)

day1_out <- file.path(json_out_dir, "day1_variable_importance.json")
day2_out <- file.path(json_out_dir, "day2_variable_importance.json")

write_json(day1_json, day1_out, pretty = TRUE, auto_unbox = TRUE)
write_json(day2_json, day2_out, pretty = TRUE, auto_unbox = TRUE)

message("Wrote variable importance JSON files:")
message("- ", day1_out)
message("- ", day2_out)

