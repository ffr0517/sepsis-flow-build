# Test/train splitting ----

# Global settings ----
source(here::here("R", "bootstrap.R"))

# Importing data ----
proc_path <- file.path(PATHS$processed, "preprocessed_subset_theta.csv")

processed_df <- read.csv(proc_path)

# Strata for splits ----
processed_df <- processed_df %>%
  group_by(ipdopd, # inpatient status
           age.group, # collapsed age
           sex, # sex
           outcome.binary, # severe illness status
  ) %>%
  mutate(combination_id = cur_group_id()) %>%
  ungroup()

processed_df <- processed_df %>%
  mutate(
    combination_id = factor(.data[["combination_id"]])
  )

table(processed_df$combination_id)

# Splitting ----
split <- initial_split(processed_df, prop = 0.7, strata = combination_id)

train_df <- training(split)
test_df  <- testing(split)

# Balance check ----
vars <- c("site",
          "ipdopd", #inpatient/outpatient
          "age.months",
          "age.group",
          "sex",
          "adm.recent", # overnight hospitalisation within last 6 months
          "wfaz",
          "cidysymp", # duration of illness (days)
          "not.alert", # not alert (AVPU < A)
          "hr.all",
          "rr.all",
          "envhtemp", # axillary temp (Celsius)
          "crt.long", # capillary refil time > 2 seconds)
          "oxy.ra", # oxygen saturation
          
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
          "CXCl10",
          
          "outcome.binary",
          "outcome.linear.2",
          "outcome.theta"
)

# summaries for test/train
train_summary <- bind_rows(lapply(vars, summarise_var, df = train_df)) |>
  mutate(set = "train_df")

test_summary <- bind_rows(lapply(vars, summarise_var, df = test_df)) |>
  mutate(set = "test_df")

summary_compare <- bind_rows(train_summary, test_summary)

# numeric var means/sigs
numeric_vars <- summary_compare |>
  filter(!is.na(mean)) |>
  distinct(variable) |>
  pull(variable)

# helper to compute Welch’s t-test p-values and standardised mean differences (SMD) to assess balance
numeric_tests <- lapply(numeric_vars, function(v) {
  x <- train_df[[v]]
  y <- test_df[[v]]
  
  tt <- t.test(x, y, var.equal = FALSE)
  
  pooled_sd <- sqrt(((sd(x, na.rm=TRUE)^2) + (sd(y, na.rm=TRUE)^2)) / 2)
  smd <- (mean(x, na.rm=TRUE) - mean(y, na.rm=TRUE)) / pooled_sd
  
  data.frame(
    variable = v,
    t_pvalue = tt$p.value,
    smd = smd
  )
}) |> bind_rows()

balance_numeric <- summary_compare |>
  filter(!is.na(mean)) |>
  dplyr::select(variable, mean, set) |>
  pivot_wider(names_from = set, values_from = mean) |>
  mutate(abs_diff = abs(train_df - test_df)) |>
  left_join(numeric_tests, by = "variable") |>
  mutate(
    t_pvalue_fdr = p.adjust(t_pvalue, method = "fdr"),
    modal_train = NA_character_,
    modal_test = NA_character_,
    prop_train = NA_real_,
    prop_test = NA_real_,
    cat_pvalue = NA_real_,
    cat_pvalue_fdr = NA_real_
  )

# categorical var modes/props/chisquare/fisher
categorical_vars <- summary_compare |>
  filter(!is.na(proportion)) |>
  distinct(variable) |>
  pull(variable)

# helper to extract modal levels, modal proportions, and run chisquare/fisher tests to quantify imbalance 
categorical_tests <- lapply(categorical_vars, function(v) {
  
  # modal levels
  tab_train <- prop.table(table(train_df[[v]], useNA="ifany"))
  tab_test  <- prop.table(table(test_df[[v]],  useNA="ifany"))
  
  modal_train <- names(which.max(tab_train))
  modal_test  <- names(which.max(tab_test))
  prop_train <- max(tab_train)
  prop_test  <- max(tab_test)
  
  # full frequency table
  full_tab <- table(
    data.frame(
      value = c(train_df[[v]], test_df[[v]]),
      set   = c(rep("train", nrow(train_df)), rep("test", nrow(test_df)))
    ),
    useNA = "ifany"
  )
  
  # chisq/Fisher
  if (all(dim(full_tab) == c(2,2)) && any(full_tab < 5)) {
    pval <- fisher.test(full_tab)$p.value
  } else {
    pval <- suppressWarnings(chisq.test(full_tab)$p.value)
  }
  
  data.frame(
    variable = v,
    modal_train = modal_train,
    modal_test = modal_test,
    prop_train = prop_train,
    prop_test  = prop_test,
    abs_diff = abs(prop_train - prop_test),
    cat_pvalue = pval
  )
}) |> bind_rows()

categorical_tests$cat_pvalue_fdr <- p.adjust(categorical_tests$cat_pvalue, method = "fdr")

balance_categorical <- categorical_tests |>
  mutate(
    t_pvalue = NA_real_,
    smd = NA_real_,
    t_pvalue_fdr = NA_real_
  )

# combined results
balance_check_all <- bind_rows(
  balance_numeric %>%
    dplyr::select(variable, abs_diff, smd, t_pvalue, t_pvalue_fdr,
           modal_train, modal_test, prop_train, prop_test,
           cat_pvalue, cat_pvalue_fdr),
  balance_categorical
) |>
  arrange(desc(abs_diff))

if (interactive()) View(balance_check_all)

# Saving simple test/train split ----
write_rds(
  train_df,
  file.path(PATHS$processed, "train.rds")
)

write_rds(
  test_df,
  file.path(PATHS$processed, "test.rds")
)

# Saving raw balance check table ----
write_rds(
  balance_check_all,
  file.path(PATHS$tables_preprocessing, "split_balance_check_table_raw.rds")
)
