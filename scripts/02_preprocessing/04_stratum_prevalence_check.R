# Per-stratum prevalence check
# Global settings ----
source(here::here("R", "bootstrap.R"))

library(rpart)
library(rpart.plot)
library(pROC)

save_dir <- PATHS$data_reference

# Setting outcome for this script ----
SELECTED_OUTCOME_VAR <- "LEVEL1_TREATMENTS_D1_SAFE_0"

OUTCOME_SYM <- rlang::sym(SELECTED_OUTCOME_VAR)

# Loading data ----
raw_train <- readRDS(file.path(PATHS$processed, "train.rds"))
raw_test <- readRDS(file.path(PATHS$processed, "test.rds"))
measures_df <- read.csv(file.path(PATHS$processed, "individual_measures.csv"))

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


save_dir <- path.expand(save_dir)

outcome_cols <- grep("^LEVEL[1-5]_TREATMENTS_D[12]_SAFE_0$", names(df), value = TRUE)

desired_order <- c(
  paste0("LEVEL", 1:5, "_TREATMENTS_D1_SAFE_0"),
  paste0("LEVEL", 1:5, "_TREATMENTS_D2_SAFE_0")
)
outcome_cols <- desired_order[desired_order %in% outcome_cols]

if (length(outcome_cols) != 10) {
  stop(
    "Expected 10 outcome columns (LEVEL1-5 for D1 and D2), found: ",
    length(outcome_cols),
    "\nFound: ",
    paste(outcome_cols, collapse = ", ")
  )
}

to_case01 <- function(y) {
  if (is.logical(y)) {
    return(as.integer(y))
  }
  if (is.numeric(y)) {
    return(as.integer(y == 1))
  }
  if (is.factor(y)) y <- as.character(y)
  y <- trimws(tolower(as.character(y)))
  as.integer(y %in% c("1", "yes", "y", "true", "case"))
}

calc_prev_by_stratum <- function(data, outcome_col, stratum_col = "stratum") {
  y_raw <- data[[outcome_col]]
  s <- data[[stratum_col]]

  y <- to_case01(y_raw)
  keep <- !is.na(y) & !is.na(s)

  cases <- tapply(y[keep], s[keep], sum)
  n <- tapply(y[keep], s[keep], length)
  prev <- cases / n

  out <- data.frame(
    stratum = names(n),
    n = as.integer(n),
    cases = as.integer(cases[names(n)]),
    prevalence = as.numeric(prev[names(n)]),
    row.names = NULL
  )

  out[order(out$stratum), ]
}

# ---- Compute + print tables (loop for computation/printing only) ----
prev_tables <- vector("list", length(outcome_cols))
names(prev_tables) <- outcome_cols

for (col in outcome_cols) {
  tbl <- calc_prev_by_stratum(df, col, "stratum")
  prev_tables[[col]] <- tbl

  cat("\n====================\n", col, "\n====================\n", sep = "")
  print(tbl, row.names = FALSE)
}

# ---- Saving (not part of the loop above) ----
prev_long <- do.call(
  rbind,
  lapply(names(prev_tables), function(outcome) {
    cbind(outcome = outcome, prev_tables[[outcome]])
  })
)
row.names(prev_long) <- NULL

saveRDS(
  prev_tables,
  file = file.path(save_dir, "prevalence_by_stratum_tables.rds")
)

write.csv(
  prev_long,
  file = file.path(save_dir, "prevalence_by_stratum_long.csv"),
  row.names = FALSE
)

cat("\nSaved:\n",
  "- ", file.path(save_dir, "prevalence_by_stratum_tables.rds"), "\n",
  "- ", file.path(save_dir, "prevalence_by_stratum_long.csv"), "\n",
  sep = ""
)

# ----
# Prevalence by inpatient status (ipdopd only)
# ----

# Clean ipdopd to explicit labels
df <- df %>%
  mutate(
    ip_status = case_when(
      ipdopd == "I" ~ "Inpatient",
      ipdopd == "O" ~ "Outpatient",
      TRUE ~ NA_character_
    )
  )

# Compute prevalence tables by inpatient status
prev_ip_tables <- vector("list", length(outcome_cols))
names(prev_ip_tables) <- outcome_cols

for (col in outcome_cols) {
  tbl <- calc_prev_by_stratum(df, col, "ip_status")
  prev_ip_tables[[col]] <- tbl

  cat("\n====================\n", col, "(by inpatient status)\n====================\n", sep = "")
  print(tbl, row.names = FALSE)
}

# Long format
prev_ip_long <- do.call(
  rbind,
  lapply(names(prev_ip_tables), function(outcome) {
    cbind(outcome = outcome, prev_ip_tables[[outcome]])
  })
)
row.names(prev_ip_long) <- NULL

# Save
saveRDS(
  prev_ip_tables,
  file = file.path(save_dir, "prevalence_by_inpatient_status_tables.rds")
)

write.csv(
  prev_ip_long,
  file = file.path(save_dir, "prevalence_by_inpatient_status_long.csv"),
  row.names = FALSE
)

cat("\nSaved:\n",
  "- ", file.path(save_dir, "prevalence_by_inpatient_status_tables.rds"), "\n",
  "- ", file.path(save_dir, "prevalence_by_inpatient_status_long.csv"), "\n",
  sep = ""
)

# ----
# Prevalence by country
# ----

# Ensure country is present and clean
df <- df %>%
  mutate(country = as.character(country))

# Compute prevalence tables by country
prev_country_tables <- vector("list", length(outcome_cols))
names(prev_country_tables) <- outcome_cols

for (col in outcome_cols) {
  tbl <- calc_prev_by_stratum(df, col, "country")
  prev_country_tables[[col]] <- tbl

  cat("\n====================\n", col, "(by country)\n====================\n", sep = "")
  print(tbl, row.names = FALSE)
}


# Long format
prev_country_long <- do.call(
  rbind,
  lapply(names(prev_country_tables), function(outcome) {
    cbind(outcome = outcome, prev_country_tables[[outcome]])
  })
)
row.names(prev_country_long) <- NULL

# Save
saveRDS(
  prev_country_tables,
  file = file.path(save_dir, "prevalence_by_country_tables.rds")
)

write.csv(
  prev_country_long,
  file = file.path(save_dir, "prevalence_by_country_long.csv"),
  row.names = FALSE
)

cat("\nSaved:\n",
  "- ", file.path(save_dir, "prevalence_by_country_tables.rds"), "\n",
  "- ", file.path(save_dir, "prevalence_by_country_long.csv"), "\n",
  sep = ""
)

# ----
# Overall treatment prevalence (whole cohort)
# ----

calc_prev_whole_cohort <- function(data, outcome_col) {
  y_raw <- data[[outcome_col]]
  y <- to_case01(y_raw)
  keep <- !is.na(y)

  cases <- sum(y[keep])
  n <- sum(keep)
  prev <- cases / n

  data.frame(
    stratum = "Whole cohort",
    n = as.integer(n),
    cases = as.integer(cases),
    prevalence = as.numeric(prev),
    row.names = NULL
  )
}

# Compute tables
prev_whole_tables <- vector("list", length(outcome_cols))
names(prev_whole_tables) <- outcome_cols

for (col in outcome_cols) {
  tbl <- calc_prev_whole_cohort(df, col)
  prev_whole_tables[[col]] <- tbl

  cat("\n====================\n", col, "(whole cohort)\n====================\n", sep = "")
  print(tbl, row.names = FALSE)
}

# Long format
prev_whole_long <- do.call(
  rbind,
  lapply(names(prev_whole_tables), function(outcome) {
    cbind(outcome = outcome, prev_whole_tables[[outcome]])
  })
)
row.names(prev_whole_long) <- NULL

# Save
saveRDS(
  prev_whole_tables,
  file = file.path(save_dir, "prevalence_whole_cohort_tables.rds")
)

write.csv(
  prev_whole_long,
  file = file.path(save_dir, "prevalence_whole_cohort_long.csv"),
  row.names = FALSE
)

cat("\nSaved:\n",
  "- ", file.path(save_dir, "prevalence_whole_cohort_tables.rds"), "\n",
  "- ", file.path(save_dir, "prevalence_whole_cohort_long.csv"), "\n",
  sep = ""
)

# ----
# Master nested prevalence object
# ----

prevalence_all <- list(
  whole = prev_whole_tables,
  country = prev_country_tables,
  inpatient_status = prev_ip_tables,
  country_x_inpatient = prev_tables
)

# Optional: enforce consistent class
class(prevalence_all) <- c("sepsis_prevalence_nested", "list")

# Save
saveRDS(
  prevalence_all,
  file = file.path(save_dir, "prevalence_all_nested.rds")
)

cat("\nSaved:\n",
  "- ", file.path(save_dir, "prevalence_all_nested.rds"), "\n",
  sep = ""
)
