# Global settings ----
source(here::here("R", "bootstrap.R"))

# Importing data ----
proc_path <- file.path(PATHS$raw, "preprocessed.csv")

processed_df <- read.csv(proc_path)

# Removing rows with no outcome ----
processed_df <- processed_df %>%
  filter(
    !is.na(outcome.linear.2),
    !is.na(outcome.binary)
  )

# Collapsing age (WHO, 2019; WHO, 2022) ----
processed_df <- processed_df %>%
  mutate(
    age.group = case_when(
      age.months < 2 ~ 0, # birth to <2 months
      age.months >= 2 & age.months < 61 ~ 1, # 2 months to ~60 months
      TRUE ~ NA_real_
    )
  )

# Collapsing resp rate (inf+child) ----
processed_df <- processed_df %>%
  mutate(
    rr.all = case_when(
      !is.na(rr.inf) ~ rr.inf,
      !is.na(rr.child) ~ rr.child,
      TRUE ~ NA_real_
    )
  )

# Collapsing hr rate (inf+child) ----
processed_df <- processed_df %>%
  mutate(
    hr.all = case_when(
      !is.na(hr.inf) ~ hr.inf,
      !is.na(hr.child) ~ hr.child,
      TRUE ~ NA_real_
    )
  )

# Retaining only variables used in analyses ----
vars <- c(
  "label",
  "site",
  "ipdopd", # inpatient/outpatient
  "age.months",
  "age.group",
  "sex",
  "adm.recent",
  "wfaz",
  "cidysymp", # symptom duration
  "not.alert",
  "hr.all",
  "rr.all",
  "envhtemp",
  "crt.long",
  "oxy.ra",
  "ANG1",
  "ANG2",
  "CHI3L",
  "CRP",
  "IL10",
  "IL1ra",
  "IL6",
  "IL8",
  "PROC",
  "STREM1",
  "TNFR1",
  "VEGFR1",
  "enescbchb1", # haemoglobin (mg/dL)
  "lblac", # lactate (mmol/L)
  "lbglu", # glucose (mmol/L)
  "supar",
  "outcome.binary",
  "outcome.linear.2"
)

processed_subset <- processed_df %>%
  dplyr::select(all_of(vars))

processed_subset_with_theta <- processed_df %>%
  dplyr::select(dplyr::all_of(vars), outcome.theta)

# Saving ----
write.csv(
  processed_subset,
  file = file.path(PATHS$processed, "preprocessed_subset.csv"),
  row.names = FALSE
)

write.csv(
  processed_subset_with_theta,
  file = file.path(PATHS$processed, "preprocessed_subset_theta.csv"),
  row.names = FALSE
)
