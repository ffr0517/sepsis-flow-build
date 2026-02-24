# Linear severity scale creation ----

# Global settings ----
source(here::here("R", "bootstrap.R"))

# Importing data ----
fu_path <- file.path(PATHS$raw, "AllFields_Follow-up_Nov2025.csv")
dis_path <- file.path(PATHS$raw, "AllFields_Discharge_Nov2025.csv")
enr_path <- file.path(PATHS$raw, "AllFields_Enrolment_Nov2025.csv")

fu_df <- as.data.frame(read.csv(fu_path))
dis_df <- as.data.frame(read.csv(dis_path))
enr_df <- as.data.frame(read.csv(enr_path))

outcomes_df <- merge(fu_df, dis_df, by = "Label", all.x = TRUE)

outcomes <- merge(enr_df, outcomes_df, by = "Label", all.x = TRUE)

outcomes <- outcomes %>%
  mutate(
    ENDAT      = as.Date(ENDAT,      format = "%d-%b-%y"),
    DCDAT      = as.Date(DCDAT,      format = "%d-%b-%y"),
    FUDIEDAT   = as.Date(FUDIEDAT,   format = "%d-%b-%y"),
    DCDIEDAT   = as.Date(DCDIEDAT,   format = "%d-%b-%y"),
    DCORGDAT   = as.Date(DCORGDAT,   format = "%d-%b-%y")
  )

# Death within the first two days binary indicators ----
measure_df <- outcomes %>%
  mutate(
    # Time differences
    days_to_discharge = as.numeric(DCDAT - ENDAT, units = "days"), 
    days_to_fudie     = as.numeric(FUDIEDAT - ENDAT, units = "days"),
    days_to_dcdie     = as.numeric(DCDIEDAT - ENDAT, units = "days"),
    
    # Evidence of death on Day 1 
    d1_death_pos = case_when(
      !is.na(days_to_fudie) & days_to_fudie <= 1 ~ TRUE,
      !is.na(days_to_dcdie) & days_to_dcdie <= 1 ~ TRUE,
      DCALI == 0 & !is.na(days_to_discharge) & days_to_discharge <= 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Evidence of survival beyond Day 1
    d1_survive_pos = case_when(
      !is.na(days_to_fudie) & days_to_fudie > 1 ~ TRUE,
      !is.na(days_to_dcdie) & days_to_dcdie > 1 ~ TRUE,
      DCALI == 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    DEATHD1 = case_when(
      d1_death_pos & !d1_survive_pos ~ 1,
      d1_survive_pos & !d1_death_pos ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Evidence of death by Day 2 
    d2_death_pos = case_when(
      FUALI == 0 ~ TRUE,
      FUD2RSN == 3 ~ TRUE, 
      !is.na(days_to_fudie) & days_to_fudie <= 2 ~ TRUE,
      !is.na(days_to_dcdie) & days_to_dcdie <= 2 ~ TRUE,
      DCALI == 0 & !is.na(days_to_discharge) & days_to_discharge <= 2 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Evidence of survival beyond Day 2 
    d2_survive_pos = case_when(
      FUALI == 1 ~ TRUE,
      DCALI == 1 ~ TRUE,
      !is.na(days_to_fudie) & days_to_fudie > 2 ~ TRUE,
      !is.na(days_to_dcdie) & days_to_dcdie > 2 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    DEATHD2 = case_when(
      d2_death_pos & !d2_survive_pos ~ 1,
      d2_survive_pos & !d2_death_pos ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    DEATHD1 = case_when(
      is.na(DEATHD1) & DEATHD2 == 0 ~ 0, 
      TRUE ~ DEATHD1 
    )
  ) %>%
  dplyr::select(Label, DEATHD1, DEATHD2)

# Mechanical ventilation, inotropes, or renal replacement therapy within the first two days binary indicators ----
measure_df <- measure_df %>%
  left_join(
    outcomes %>%
      transmute(
        Label,
        
        d1_any_ones = pmax(TMD1MEC, TMD1IN, TMD1RRT, na.rm = TRUE),
        
        d1_any_nas = is.na(TMD1MEC) | is.na(TMD1IN) | is.na(TMD1RRT),
        
        d1_all_zero = (rowSums(cbind(
          replace(TMD1MEC, is.na(TMD1MEC), 0),
          replace(TMD1IN,  is.na(TMD1IN),  0),
          replace(TMD1RRT, is.na(TMD1RRT), 0)
        )) == 0),
        
        
        d2_any_ones = pmax(TMD2MEC, TMD2IN, TMD2RRT, na.rm = TRUE),
        
        d2_any_nas = is.na(TMD2MEC) | is.na(TMD2IN) | is.na(TMD2RRT),
        
        d2_all_zero = (rowSums(cbind(
          replace(TMD2MEC, is.na(TMD2MEC), 0),
          replace(TMD2IN,  is.na(TMD2IN),  0),
          replace(TMD2RRT, is.na(TMD2RRT), 0)
        )) == 0)
      ) %>%
      mutate(
        
        LEVEL1_TREATMENTS_D1 = case_when(
          d1_any_ones == 1 ~ 1,
          d1_all_zero & !d1_any_nas ~ 0,
          d1_all_zero & d1_any_nas ~ NA_real_,
          TRUE ~ NA_real_
        ),
        
        LEVEL1_TREATMENTS_D2 = case_when(
          d2_any_ones == 1 ~ 1,
          d2_all_zero & !d2_any_nas ~ 0,
          d2_all_zero & d2_any_nas ~ NA_real_,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::select(Label, LEVEL1_TREATMENTS_D1, LEVEL1_TREATMENTS_D2),
    by = "Label"
  )

# CPAP or IV fluid bolus within the first two days binary indicator ----
measure_df <- measure_df %>%
  left_join(
    outcomes %>%
      transmute(
        Label,
        any_ones_d1 = pmax(TMD1CPAP, TMD1FB, na.rm = TRUE),
        
        any_nas_d1 = is.na(TMD1CPAP) | is.na(TMD1FB),
        
        all_zero_d1 = (rowSums(cbind(
          replace(TMD1CPAP, is.na(TMD1CPAP), 0),
          replace(TMD1FB,   is.na(TMD1FB),   0)
        )) == 0),
        
        any_ones_d2 = pmax(TMD2CPAP, TMD2FB, na.rm = TRUE),
        
        any_nas_d2 = is.na(TMD2CPAP) | is.na(TMD2FB),
        
        all_zero_d2 = (rowSums(cbind(
          replace(TMD2CPAP, is.na(TMD2CPAP), 0),
          replace(TMD2FB,   is.na(TMD2FB),   0)
        )) == 0)
      ) %>%
      mutate(
        
        LEVEL2_TREATMENTS_D1 = case_when(
          any_ones_d1 == 1 ~ 1,
          all_zero_d1 & !any_nas_d1 ~ 0,
          all_zero_d1 & any_nas_d1 ~ NA_real_,
          TRUE ~ NA_real_
        ),
        
        LEVEL2_TREATMENTS_D2 = case_when(
          any_ones_d2 == 1 ~ 1,
          all_zero_d2 & !any_nas_d2 ~ 0,
          all_zero_d2 & any_nas_d2 ~ NA_real_,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::select(Label, LEVEL2_TREATMENTS_D1, LEVEL2_TREATMENTS_D2),
    by = "Label"
  )

# ICU admission (clinical reason) within the first two days binary indicator ----
measure_df <- measure_df %>%
  left_join(
    outcomes %>%
      transmute(
        Label,
        
        # Raw inputs
        d1_icu    = TMD1ICU,
        d2_icu    = TMD2ICU,
        d1_reason = TMD1ICUCLI,
        d2_reason = TMD2ICUCLI
      ) %>%
      
      mutate(
        LEVEL3_TREATMENTS_D1 = case_when(
          d1_icu == 1 & d1_reason == 1 ~ 1, 
          d1_icu == 1 & d1_reason == 0 ~ 0, 
          d1_icu == 1 & is.na(d1_reason) ~ NA_real_, 
          d1_icu == 0 & !is.na(d1_icu) ~ 0, 
          TRUE ~ NA_real_ 
        )
      ) %>%
      
      mutate(
        LEVEL3_TREATMENTS_D2 = case_when(
          d2_icu == 1 & d2_reason == 1 ~ 1, 
          d2_icu == 1 & d2_reason == 0 ~ 0,
          d2_icu == 1 & is.na(d2_reason) ~ NA_real_, 
          d2_icu == 0 & !is.na(d2_icu) ~ 0,
          TRUE ~ NA_real_ 
        )
      ) %>%
      
      dplyr::select(Label, LEVEL3_TREATMENTS_D1, LEVEL3_TREATMENTS_D2),
    
    by = "Label"
  )


# O2 via face mask or nasal cannula within the first two days binary indicator ----
measure_df <- measure_df %>%
  left_join(
    outcomes %>%
      transmute(
        Label,
        
        d1_any_ones = pmax(
          TMD1OXYF, 
          TMD1OXYNC,
          na.rm = TRUE
        ),
        
        d1_any_nas = is.na(TMD1OXYF) | is.na(TMD1OXYNC),
        
        d1_all_zero = (rowSums(cbind(
          replace(TMD1OXYF,  is.na(TMD1OXYF),  0),
          replace(TMD1OXYNC, is.na(TMD1OXYNC), 0)
        )) == 0),
        
        d2_any_ones = pmax(
          TMD2OXYF, 
          TMD2OXYNC,
          na.rm = TRUE
        ),
        
        d2_any_nas = is.na(TMD2OXYF) | is.na(TMD2OXYNC),
        
        d2_all_zero = (rowSums(cbind(
          replace(TMD2OXYF,  is.na(TMD2OXYF),  0),
          replace(TMD2OXYNC, is.na(TMD2OXYNC), 0)
        )) == 0)
      ) %>%
      
      mutate(
        
        LEVEL4_TREATMENTS_D1 = case_when(
          d1_any_ones == 1 ~ 1,
          d1_all_zero & !d1_any_nas ~ 0,
          d1_all_zero & d1_any_nas  ~ NA_real_,
          TRUE ~ NA_real_
        ),
        
        LEVEL4_TREATMENTS_D2 = case_when(
          d2_any_ones == 1 ~ 1,
          d2_all_zero & !d2_any_nas ~ 0,
          d2_all_zero & d2_any_nas  ~ NA_real_,
          TRUE ~ NA_real_
        )
      ) %>%
      
      dplyr::select(Label, LEVEL4_TREATMENTS_D1, LEVEL4_TREATMENTS_D2),
    
    by = "Label"
  )


# Non-bolused IV fluids within the first two days binary indicator ----
measure_df <- measure_df %>%
  left_join(
    outcomes %>%
      transmute(
        Label,
        
        d1_one = TMD1MIF == 1,
        d2_one = TMD2MIF == 1,
        
        d1_na = is.na(TMD1MIF),
        d2_na = is.na(TMD2MIF),
        
        d1_zero = (TMD1MIF == 0),
        d2_zero = (TMD2MIF == 0)
      ) %>%
      mutate(
        
        LEVEL5_TREATMENTS_D1 = case_when(
          d1_one ~ 1,
          d1_zero & !d1_na ~ 0,
          TRUE ~ NA_real_
        ),
        
        LEVEL5_TREATMENTS_D2 = case_when(
          d2_one ~ 1,
          d2_zero & !d2_na ~ 0,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::select(Label, LEVEL5_TREATMENTS_D1, LEVEL5_TREATMENTS_D2),
    by = "Label"
  )


# Generic organ support + discharge to die at home indicators ----
measure_df <- measure_df %>%
  left_join(
    outcomes %>%
      mutate(
        days_to_discharge = as.numeric(DCDAT - ENDAT, units = "days"),
        days_to_orgsup    = as.numeric(DCORGDAT - ENDAT, units = "days"),
        
        d2_orgsup_pos = case_when(
          DCORG == 1 & !is.na(days_to_orgsup) & days_to_orgsup <= 2 ~ TRUE,
          TRUE ~ FALSE
        ),
        
        d2_orgsup_neg = case_when(
          DCORG == 1 & !is.na(days_to_orgsup) & days_to_orgsup > 2 ~ TRUE,
          DCORG == 0 ~ TRUE,
          TRUE ~ FALSE
        ),
        
        GENERIC_ORG_SUP = case_when(
          d2_orgsup_pos & !d2_orgsup_neg ~ 1,
          d2_orgsup_neg & !d2_orgsup_pos ~ 0,
          TRUE ~ NA_real_
        ),
        
        d2_dcdie_pos = case_when(
          DCDIE == 1 & !is.na(days_to_discharge) & days_to_discharge <= 2 ~ TRUE,
          TRUE ~ FALSE
        ),
        
        d2_dcdie_neg = case_when(
          DCDIE == 1 & !is.na(days_to_discharge) & days_to_discharge > 2 ~ TRUE,
          DCDIE == 0 ~ TRUE,
          TRUE ~ FALSE
        ),
        
        DISCHARGE_DIE = case_when(
          d2_dcdie_pos & !d2_dcdie_neg ~ 1,
          d2_dcdie_neg & !d2_dcdie_pos ~ 0,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::select(Label, GENERIC_ORG_SUP, DISCHARGE_DIE),
    by = "Label"
  )

# Safe 0 assignment ----
measure_df <- measure_df %>%
  left_join(
    outcomes %>%
      dplyr::select(Label, IPDOPD, FUCARE, FUADMYN, FUMEDYN, FUD2ORG,
             ENDAT, DCDAT, DCORG),
    by = "Label"
  ) %>%
  mutate(
    days_to_discharge = as.numeric(DCDAT - ENDAT, units = "days"),
    inpatient_d1_discharge = IPDOPD == "I" & !is.na(days_to_discharge) &
      days_to_discharge %in% c(0, 1)
  ) %>%
  mutate(
    
    safe_zero_outpatient = case_when(
      IPDOPD == "O" & FUCARE == 0 ~ TRUE,
      IPDOPD == "O" & FUCARE == 1 & FUADMYN == 0 & FUMEDYN == 0 ~ TRUE,
      IPDOPD == "O" & FUCARE == 1 & FUMEDYN == 1 & FUADMYN == 0 ~ TRUE,
      IPDOPD == "O" & FUD2ORG == 0 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    safe_zero_inpatient_d2 = inpatient_d1_discharge
  ) %>%
  mutate(
    safe_zero_d1 = safe_zero_outpatient
    ,
    safe_zero_d2 = safe_zero_outpatient | safe_zero_inpatient_d2
  ) %>%
  mutate(
    
    DEATHD1_SAFE_0 = if_else(is.na(DEATHD1) & safe_zero_d1, 0, DEATHD1),
    DEATHD2_SAFE_0 = if_else(is.na(DEATHD2) & safe_zero_d2, 0, DEATHD2),
    
    LEVEL1_TREATMENTS_D1_SAFE_0 =
      if_else(is.na(LEVEL1_TREATMENTS_D1) & safe_zero_d1, 0, LEVEL1_TREATMENTS_D1),
    LEVEL1_TREATMENTS_D2_SAFE_0 =
      if_else(is.na(LEVEL1_TREATMENTS_D2) & safe_zero_d2, 0, LEVEL1_TREATMENTS_D2),
    
    LEVEL2_TREATMENTS_D1_SAFE_0 =
      if_else(is.na(LEVEL2_TREATMENTS_D1) & safe_zero_d1, 0, LEVEL2_TREATMENTS_D1),
    LEVEL2_TREATMENTS_D2_SAFE_0 =
      if_else(is.na(LEVEL2_TREATMENTS_D2) & safe_zero_d2, 0, LEVEL2_TREATMENTS_D2),
    
    LEVEL3_TREATMENTS_D1_SAFE_0 =
      if_else(is.na(LEVEL3_TREATMENTS_D1) & safe_zero_d1, 0, LEVEL3_TREATMENTS_D1),
    LEVEL3_TREATMENTS_D2_SAFE_0 =
      if_else(is.na(LEVEL3_TREATMENTS_D2) & safe_zero_d2, 0, LEVEL3_TREATMENTS_D2),
    
    LEVEL4_TREATMENTS_D1_SAFE_0 =
      if_else(is.na(LEVEL4_TREATMENTS_D1) & safe_zero_d1, 0, LEVEL4_TREATMENTS_D1),
    LEVEL4_TREATMENTS_D2_SAFE_0 =
      if_else(is.na(LEVEL4_TREATMENTS_D2) & safe_zero_d2, 0, LEVEL4_TREATMENTS_D2),
    
    LEVEL5_TREATMENTS_D1_SAFE_0 =
      if_else(is.na(LEVEL5_TREATMENTS_D1) & safe_zero_d1, 0, LEVEL5_TREATMENTS_D1),
    LEVEL5_TREATMENTS_D2_SAFE_0 =
      if_else(is.na(LEVEL5_TREATMENTS_D2) & safe_zero_d2, 0, LEVEL5_TREATMENTS_D2),
    
    DISCHARGE_DIE_SAFE_0 =
      if_else(
        is.na(DISCHARGE_DIE) & safe_zero_d2 & IPDOPD == "O",
        0,
        DISCHARGE_DIE
      ),
    
    GENERIC_ORG_SUP_SAFE_0 =
      if_else(
        is.na(GENERIC_ORG_SUP) & (safe_zero_d2 & IPDOPD == "O" | DCORG == 0),
        0,
        GENERIC_ORG_SUP
      )
  )

write.csv(
  measure_df,
  file = file.path(PATHS$processed, "individual_measures.csv"),
  row.names = FALSE
)

# Score assignment ----
measure_df <- measure_df %>%
  mutate(
    # Day 1
    SCORE_D1 = case_when(
      DEATHD1 == 1 ~ 10^5,
      LEVEL1_TREATMENTS_D1 == 1 ~ 10^4,
      LEVEL2_TREATMENTS_D1 == 1 ~ 10^3,
      LEVEL3_TREATMENTS_D1 == 1 ~ 10^2,
      LEVEL4_TREATMENTS_D1 == 1 ~ 10^1,
      LEVEL5_TREATMENTS_D1 == 1 ~ 10^0,
      
      LEVEL1_TREATMENTS_D1 == 0 &
        LEVEL2_TREATMENTS_D1 == 0 &
        LEVEL3_TREATMENTS_D1 == 0 &
        LEVEL4_TREATMENTS_D1 == 0 &
        LEVEL5_TREATMENTS_D1 == 0 &
        DEATHD1 == 0 ~ 0,
      
      TRUE ~ NA_real_ 
    ),
    
    SCORE_D2 = case_when(
      DEATHD2 == 1 ~ 10^5,
      LEVEL1_TREATMENTS_D2 == 1 ~ 10^4,
      LEVEL2_TREATMENTS_D2 == 1 ~ 10^3,
      LEVEL3_TREATMENTS_D2 == 1 ~ 10^2,
      LEVEL4_TREATMENTS_D2 == 1 ~ 10^1,
      LEVEL5_TREATMENTS_D2 == 1 ~ 10^0,
      
      LEVEL1_TREATMENTS_D2 == 0 &
        LEVEL2_TREATMENTS_D2 == 0 &
        LEVEL3_TREATMENTS_D2 == 0 &
        LEVEL4_TREATMENTS_D2 == 0 &
        LEVEL5_TREATMENTS_D2 == 0 &
        DEATHD2 == 0 ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    FINAL_SCORE = SCORE_D1 + SCORE_D2
  )

measure_df <- measure_df %>%
  mutate(
    SCORE_D1_SAFE_0 = case_when(
      DEATHD1_SAFE_0 == 1 ~ 10^5,
      LEVEL1_TREATMENTS_D1_SAFE_0 == 1 ~ 10^4,
      LEVEL2_TREATMENTS_D1_SAFE_0 == 1 ~ 10^3,
      LEVEL3_TREATMENTS_D1_SAFE_0 == 1 ~ 10^2,
      LEVEL4_TREATMENTS_D1_SAFE_0 == 1 ~ 10^1,
      LEVEL5_TREATMENTS_D1_SAFE_0 == 1 ~ 10^0,
      
      LEVEL1_TREATMENTS_D1_SAFE_0 == 0 &
        LEVEL2_TREATMENTS_D1_SAFE_0 == 0 &
        LEVEL3_TREATMENTS_D1_SAFE_0 == 0 &
        LEVEL4_TREATMENTS_D1_SAFE_0 == 0 &
        LEVEL5_TREATMENTS_D1_SAFE_0 == 0 &
        DEATHD1_SAFE_0 == 0 ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    SCORE_D2_SAFE_0 = case_when(
      DEATHD2_SAFE_0 == 1 ~ 10^5,
      LEVEL1_TREATMENTS_D2_SAFE_0 == 1 ~ 10^4,
      LEVEL2_TREATMENTS_D2_SAFE_0 == 1 ~ 10^3,
      LEVEL3_TREATMENTS_D2_SAFE_0 == 1 ~ 10^2,
      LEVEL4_TREATMENTS_D2_SAFE_0 == 1 ~ 10^1,
      LEVEL5_TREATMENTS_D2_SAFE_0 == 1 ~ 10^0,
      
      LEVEL1_TREATMENTS_D2_SAFE_0 == 0 &
        LEVEL2_TREATMENTS_D2_SAFE_0 == 0 &
        LEVEL3_TREATMENTS_D2_SAFE_0 == 0 &
        LEVEL4_TREATMENTS_D2_SAFE_0 == 0 &
        LEVEL5_TREATMENTS_D2_SAFE_0 == 0 &
        DEATHD2_SAFE_0 == 0 ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    FINAL_SCORE_SAFE_0 = SCORE_D1_SAFE_0 + SCORE_D2_SAFE_0
  )

# Adding rank 
measure_df <- measure_df %>%
  mutate(
    TOTAL_SCORE = SCORE_D1 + SCORE_D2,
    RANK_SCORE = min_rank(TOTAL_SCORE)
  )

# Number of unique theoretical values of score
## Can only be 0, 1, 10, 100, 1000, 10000 or 100000 on each day (n = 7 unique possible values per day)
vals <- c(0, 1, 10, 100, 1000, 10000, 100000)
n <- length(vals)

numerator <- n * (n + 1) ## +1 because each element can pair with itself

denominator <- 2 ## unordered pairs treat (a,b) and (b,a) as the same thing, so halve

theoretical_possible_values <- numerator / denominator ## 28

# all pathways
pathways <- expand.grid(
  D1 = vals,
  D2 = vals
) |>
  transform(FINAL = D1 + D2)

# ordered 28 unique values
unique_values <- sort(unique(pathways$FINAL))

# map to 1-28
final_score_map <- data.frame(
  FINAL = unique_values,
  RANK  = seq_along(unique_values)
)

measure_df <- measure_df %>%
  left_join(final_score_map %>% rename(RANK_MAP = RANK),
            by = c("FINAL_SCORE" = "FINAL")) %>%
  mutate(
    FINAL_SCORE_MAPPED  = RANK_MAP,
    FINAL_SCORE_BOUNDED = FINAL_SCORE_MAPPED / length(unique(final_score_map$RANK))
  ) %>%
  dplyr::select(-RANK_MAP)

measure_df <- measure_df %>%
  mutate(
    TOTAL_SCORE_SAFE_0 = SCORE_D1_SAFE_0 + SCORE_D2_SAFE_0,
    RANK_SCORE_SAFE_0  = min_rank(TOTAL_SCORE_SAFE_0)
  ) %>%
  left_join(final_score_map %>% rename(RANK_MAP = RANK),
            by = c("TOTAL_SCORE_SAFE_0" = "FINAL")) %>%
  mutate(
    FINAL_SCORE_MAPPED_SAFE_0  = RANK_MAP,
    FINAL_SCORE_BOUNDED_SAFE_0 = FINAL_SCORE_MAPPED_SAFE_0 / length(unique(final_score_map$RANK))
  ) %>%
  dplyr::select(-RANK_MAP)


# Joining binary and categorical outcomes ----
proc_path <- file.path(PATHS$raw, "predictive.analysis.univariate_17Oct2024.xlsx")

processed_df <- read_excel(proc_path, sheet = 1) |> 
  as.data.frame()

measure_df <- measure_df %>%
  dplyr::left_join(
    processed_df %>% dplyr::select(label, outcome.cat, outcome.binary),
    by = c("Label" = "label")
  )

# Measure only data for item-level validity checks ----
measures_alone <- measure_df[, c(
  "DEATHD1_SAFE_0", 
  "LEVEL1_TREATMENTS_D1_SAFE_0",
  "LEVEL2_TREATMENTS_D1_SAFE_0",
  "LEVEL3_TREATMENTS_D1_SAFE_0",
  "LEVEL4_TREATMENTS_D1_SAFE_0",
  "LEVEL5_TREATMENTS_D1_SAFE_0",
  "DEATHD2_SAFE_0",
  "LEVEL1_TREATMENTS_D2_SAFE_0",
  "LEVEL2_TREATMENTS_D2_SAFE_0",
  "LEVEL3_TREATMENTS_D2_SAFE_0",
  "LEVEL4_TREATMENTS_D2_SAFE_0",
  "LEVEL5_TREATMENTS_D2_SAFE_0",
  "DISCHARGE_DIE_SAFE_0",
  "GENERIC_ORG_SUP_SAFE_0"
)] 

day1_complete <- measure_df[, c(
  "DEATHD1_SAFE_0", 
  "LEVEL1_TREATMENTS_D1_SAFE_0",
  "LEVEL2_TREATMENTS_D1_SAFE_0",
  "LEVEL3_TREATMENTS_D1_SAFE_0",
  "LEVEL4_TREATMENTS_D1_SAFE_0",
  "LEVEL5_TREATMENTS_D1_SAFE_0"
)] |>
  na.omit()

day2_complete <- measure_df[, c(
  "DEATHD2_SAFE_0",
  "LEVEL1_TREATMENTS_D2_SAFE_0",
  "LEVEL2_TREATMENTS_D2_SAFE_0",
  "LEVEL3_TREATMENTS_D2_SAFE_0",
  "LEVEL4_TREATMENTS_D2_SAFE_0",
  "LEVEL5_TREATMENTS_D2_SAFE_0"
)] |>
  na.omit()

# Cronbachs alpha -----
cr_alpha_d1 <- alpha(day1_complete)
cr_alpha_d2 <- alpha(day2_complete)

cr_alpha_d1$total$raw_alpha
cr_alpha_d2$total$raw_alpha

# McDonalds omega -----
mc_omega_d1 <- omega(day1_complete, nfactors = 1)
mc_omega_d2 <- omega(day2_complete, nfactors = 1)

# 2PL IRT ----
irt_2pl <- mirt(
  data = measures_alone,
  model = 1, # one latent trait
  itemtype = "2PL", # binary 2-parameter logistic
  technical = list(NCYCLES = 2000)
)

summary(irt_2pl)

plot(irt_2pl, type = "trace")

coef_tab <- coef(irt_2pl, IRTpars = TRUE, simplify = TRUE)$items

coef_df <- as.data.frame(coef_tab)
coef_df$item <- rownames(coef_df)
rownames(coef_df) <- NULL

lookup <- c(
  "DEATHD1_SAFE_0" = "Death (Day 1)",
  "LEVEL1_TREATMENTS_D1_SAFE_0" = "Mechanical Ventilation* (Day 1)",
  "LEVEL2_TREATMENTS_D1_SAFE_0" = "CPAP or IV Fluid Bolus (Day 1)",
  "LEVEL3_TREATMENTS_D1_SAFE_0" = "ICU Admission* (Day 1)",
  "LEVEL4_TREATMENTS_D1_SAFE_0" = "Oxygen* (Day 1)",
  "LEVEL5_TREATMENTS_D1_SAFE_0" = "Non-bolused IV Fluids (Day 1)",
  
  "DEATHD2_SAFE_0" = "Death (Day 2)",
  "LEVEL1_TREATMENTS_D2_SAFE_0" = "Mechanical Ventilation* (Day 2)",
  "LEVEL2_TREATMENTS_D2_SAFE_0" = "CPAP or IV Fluid Bolus (Day 2)",
  "LEVEL3_TREATMENTS_D2_SAFE_0" = "ICU Admission* (Day 2)",
  "LEVEL4_TREATMENTS_D2_SAFE_0" = "Oxygen* (Day 2)",
  "LEVEL5_TREATMENTS_D2_SAFE_0" = "Non-bolused IV Fluids (Day 2)",
  
  "DISCHARGE_DIE_SAFE_0" = "Discharged to die at home (Day 1 | Day 2)",
  "GENERIC_ORG_SUP_SAFE_0" = "Organ support flag (Day 1 | Day 2)"
)


lookup_order <- c(
  "DEATHD1_SAFE_0" = 100000,
  "DEATHD2_SAFE_0" = 100000,
  "DISCHARGE_DIE_SAFE_0" = 100000,
  
  "LEVEL1_TREATMENTS_D1_SAFE_0" = 10000,
  "LEVEL1_TREATMENTS_D2_SAFE_0" = 10000,
  
  "LEVEL2_TREATMENTS_D1_SAFE_0" = 1000,
  "LEVEL2_TREATMENTS_D2_SAFE_0" = 1000,
  "GENERIC_ORG_SUP_SAFE_0" = 1000,
  
  "LEVEL3_TREATMENTS_D1_SAFE_0" = 100,
  "LEVEL3_TREATMENTS_D2_SAFE_0" = 100,
  
  "LEVEL4_TREATMENTS_D1_SAFE_0" = 10,
  "LEVEL4_TREATMENTS_D2_SAFE_0" = 10,
  
  "LEVEL5_TREATMENTS_D1_SAFE_0" = 1,
  "LEVEL5_TREATMENTS_D2_SAFE_0" = 1
)

coef_df <- coef_df %>%
  mutate(order = lookup_order[item])

coef_df <- coef_df %>%
  mutate(name = lookup[item])

theta_scores <- fscores(irt_2pl, method = "EAP")
theta_df <- data.frame(
  Label = measure_df$Label,
  theta = as.numeric(theta_scores)
)

measure_df <- measure_df %>%
  left_join(theta_df, by = "Label")

# Difficulty by item
ggplot(coef_df, aes(x = b, y = reorder(name, order), colour = factor(order))) +
  geom_point(size = 3) +
  scale_colour_manual(
    values = c(
      "1" = "#4575b4",
      "10" = "#74add1",
      "100" = "#abd9e9",
      "1000" = "#fdae61",
      "10000" = "#f46d43",
      "100000" = "#d73027"
    )
  ) +
  labs(
    x = "Difficulty (b)",
    y = "Item",
    title = "Item difficulty parameters",
    subtitle = "Ordered by Linear Scale Weight*",
    colour = "Weight",
    caption = "*Discharge and organ support indicators are proposed weight"
  ) +
  theme_minimal()

# Discrimination by item
ggplot(coef_df, aes(x = a, y = reorder(name, order), colour = factor(order))) +
  geom_point(size = 3) +
  scale_colour_manual(
    values = c(
      "1" = "#4575b4",
      "10" = "#74add1",
      "100" = "#abd9e9",
      "1000" = "#fdae61",
      "10000" = "#f46d43",
      "100000" = "#d73027"
    )
  ) +
  labs(
    x = "Discrimination (b)",
    y = "Item",
    title = "Item discrimination parameters",
    subtitle = "Ordered by Linear Scale Weight",
    colour = "Weight"
  ) +
  theme_minimal()

# Amended score assignment ----
measure_df <- measure_df %>%
  mutate(
    SCORE_D1 = case_when(
      DEATHD1 == 1 ~ 10^5,
      DISCHARGE_DIE == 1 ~ 10^5,       
      LEVEL1_TREATMENTS_D1 == 1 ~ 10^4,
      LEVEL2_TREATMENTS_D1 == 1 ~ 10^3,
      GENERIC_ORG_SUP == 1 ~ 10^3,          
      LEVEL3_TREATMENTS_D1 == 1 ~ 10^2,
      LEVEL4_TREATMENTS_D1 == 1 ~ 10^1,
      LEVEL5_TREATMENTS_D1 == 1 ~ 10^0,
      
      LEVEL1_TREATMENTS_D1 == 0 &
        LEVEL2_TREATMENTS_D1 == 0 &
        LEVEL3_TREATMENTS_D1 == 0 &
        LEVEL4_TREATMENTS_D1 == 0 &
        LEVEL5_TREATMENTS_D1 == 0 &
        DEATHD1 == 0 &
        DISCHARGE_DIE == 0 &                     
        GENERIC_ORG_SUP == 0 ~ 0,                
      
      TRUE ~ NA_real_ 
    ),
    
    SCORE_D2 = case_when(
      DEATHD2 == 1 ~ 10^5,
      DISCHARGE_DIE == 1 ~ 10^5,                 
      LEVEL1_TREATMENTS_D2 == 1 ~ 10^4,
      LEVEL2_TREATMENTS_D2 == 1 ~ 10^3,
      GENERIC_ORG_SUP == 1 ~ 10^3,               
      LEVEL3_TREATMENTS_D2 == 1 ~ 10^2,
      LEVEL4_TREATMENTS_D2 == 1 ~ 10^1,
      LEVEL5_TREATMENTS_D2 == 1 ~ 10^0,
      
      LEVEL1_TREATMENTS_D2 == 0 &
        LEVEL2_TREATMENTS_D2 == 0 &
        LEVEL3_TREATMENTS_D2 == 0 &
        LEVEL4_TREATMENTS_D2 == 0 &
        LEVEL5_TREATMENTS_D2 == 0 &
        DEATHD2 == 0 &
        DISCHARGE_DIE == 0 &                     
        GENERIC_ORG_SUP == 0 ~ 0,                
      
      TRUE ~ NA_real_
    ),
    
    FINAL_SCORE = SCORE_D1 + SCORE_D2
  )

measure_df <- measure_df %>%
  mutate(
    SCORE_D1_SAFE_0 = case_when(
      DEATHD1_SAFE_0 == 1 ~ 10^5,
      DISCHARGE_DIE_SAFE_0 == 1 ~ 10^5,          
      LEVEL1_TREATMENTS_D1_SAFE_0 == 1 ~ 10^4,
      LEVEL2_TREATMENTS_D1_SAFE_0 == 1 ~ 10^3,
      GENERIC_ORG_SUP_SAFE_0 == 1 ~ 10^3,        
      LEVEL3_TREATMENTS_D1_SAFE_0 == 1 ~ 10^2,
      LEVEL4_TREATMENTS_D1_SAFE_0 == 1 ~ 10^1,
      LEVEL5_TREATMENTS_D1_SAFE_0 == 1 ~ 10^0,
      
      LEVEL1_TREATMENTS_D1_SAFE_0 == 0 &
        LEVEL2_TREATMENTS_D1_SAFE_0 == 0 &
        LEVEL3_TREATMENTS_D1_SAFE_0 == 0 &
        LEVEL4_TREATMENTS_D1_SAFE_0 == 0 &
        LEVEL5_TREATMENTS_D1_SAFE_0 == 0 &
        DEATHD1_SAFE_0 == 0 &
        DISCHARGE_DIE_SAFE_0 == 0 &              
        GENERIC_ORG_SUP_SAFE_0 == 0 ~ 0,         
      
      TRUE ~ NA_real_
    ),
    
    SCORE_D2_SAFE_0 = case_when(
      DEATHD2_SAFE_0 == 1 ~ 10^5,
      DISCHARGE_DIE_SAFE_0 == 1 ~ 10^5,          
      LEVEL1_TREATMENTS_D2_SAFE_0 == 1 ~ 10^4,
      LEVEL2_TREATMENTS_D2_SAFE_0 == 1 ~ 10^3,
      GENERIC_ORG_SUP_SAFE_0 == 1 ~ 10^3,        
      LEVEL3_TREATMENTS_D2_SAFE_0 == 1 ~ 10^2,
      LEVEL4_TREATMENTS_D2_SAFE_0 == 1 ~ 10^1,
      LEVEL5_TREATMENTS_D2_SAFE_0 == 1 ~ 10^0,
      
      LEVEL1_TREATMENTS_D2_SAFE_0 == 0 &
        LEVEL2_TREATMENTS_D2_SAFE_0 == 0 &
        LEVEL3_TREATMENTS_D2_SAFE_0 == 0 &
        LEVEL4_TREATMENTS_D2_SAFE_0 == 0 &
        LEVEL5_TREATMENTS_D2_SAFE_0 == 0 &
        DEATHD2_SAFE_0 == 0 &
        DISCHARGE_DIE_SAFE_0 == 0 &              
        GENERIC_ORG_SUP_SAFE_0 == 0 ~ 0,         
      
      TRUE ~ NA_real_
    ),
    
    FINAL_SCORE_SAFE_0 = SCORE_D1_SAFE_0 + SCORE_D2_SAFE_0
  )


# Adding rank 
measure_df <- measure_df %>%
  mutate(
    TOTAL_SCORE = SCORE_D1 + SCORE_D2,
    RANK_SCORE = min_rank(TOTAL_SCORE)
  )


vals <- c(0, 1, 10, 100, 1000, 10000, 100000)
n <- length(vals)

numerator <- n * (n + 1) 

denominator <- 2 

theoretical_possible_values <- numerator / denominator ## 28

pathways <- expand.grid(
  D1 = vals,
  D2 = vals
) |>
  transform(FINAL = D1 + D2)

unique_values <- sort(unique(pathways$FINAL))

final_score_map <- data.frame(
  FINAL = unique_values,
  RANK  = seq_along(unique_values)
)

measure_df <- measure_df %>%
  left_join(final_score_map %>% rename(RANK_MAP = RANK),
            by = c("FINAL_SCORE" = "FINAL")) %>%
  mutate(
    FINAL_SCORE_MAPPED  = RANK_MAP,
    FINAL_SCORE_BOUNDED = FINAL_SCORE_MAPPED / length(unique(final_score_map$RANK))
  ) %>%
  dplyr::select(-RANK_MAP)

measure_df <- measure_df %>%
  mutate(
    TOTAL_SCORE_SAFE_0 = SCORE_D1_SAFE_0 + SCORE_D2_SAFE_0,
    RANK_SCORE_SAFE_0  = min_rank(TOTAL_SCORE_SAFE_0)
  ) %>%
  left_join(final_score_map %>% rename(RANK_MAP = RANK),
            by = c("TOTAL_SCORE_SAFE_0" = "FINAL")) %>%
  mutate(
    FINAL_SCORE_MAPPED_SAFE_0  = RANK_MAP,
    FINAL_SCORE_BOUNDED_SAFE_0 = FINAL_SCORE_MAPPED_SAFE_0 / length(unique(final_score_map$RANK))
  ) %>%
  dplyr::select(-RANK_MAP)


# Adding to processed data ----
processed_df <- processed_df %>%
  left_join(
    measure_df %>% 
      dplyr::select(Label, FINAL_SCORE_BOUNDED_SAFE_0, theta),
    by = c("label" = "Label")
  ) %>%
  rename(
    outcome.linear.2 = FINAL_SCORE_BOUNDED_SAFE_0,
    outcome.theta = theta
  )

write.csv(
  processed_df,
  file = file.path(PATHS$raw, "preprocessed.csv"),
  row.names = FALSE
)