# Global settings ----
source(here::here("R", "bootstrap.R"))

library(rpart)
library(rpart.plot)
library(pROC)

# Loading data ----
save_dir <- PATHS$ensemble_workspace

files <- list.files(
  path = save_dir,
  pattern = "_pred_training_data\\.rds$",
  full.names = TRUE
)

for (f in files) {
  fname <- basename(f)
  
  x <- sub("^LEVEL([0-9]+)_.*$", "\\1", fname)
  
  obj_name <- paste0("df", x)
  
  assign(
    x = obj_name,
    value = readRDS(f),
    envir = .GlobalEnv
  )
}

# Replacing actual treaments with predicted treatments ----
df1$LEVEL1_TREATMENTS_D1_SAFE_0 <- df1$PRED_LEVEL1_TREATMENTS_D1_SAFE_0
df1 <- df1[, !(grepl("_SAFE_0$", names(df1)) & names(df1) != "LEVEL1_TREATMENTS_D1_SAFE_0")]

df2$LEVEL2_TREATMENTS_D1_SAFE_0 <- df2$PRED_LEVEL2_TREATMENTS_D1_SAFE_0
df2 <- df2[, !(grepl("_SAFE_0$", names(df2)) & names(df2) != "LEVEL2_TREATMENTS_D1_SAFE_0")]

df3$LEVEL3_TREATMENTS_D1_SAFE_0 <- df3$PRED_LEVEL3_TREATMENTS_D1_SAFE_0
df3 <- df3[, !(grepl("_SAFE_0$", names(df3)) & names(df3) != "LEVEL3_TREATMENTS_D1_SAFE_0")]

df4$LEVEL4_TREATMENTS_D1_SAFE_0 <- df4$PRED_LEVEL4_TREATMENTS_D1_SAFE_0
df4 <- df4[, !(grepl("_SAFE_0$", names(df4)) & names(df4) != "LEVEL4_TREATMENTS_D1_SAFE_0")]

df5$LEVEL5_TREATMENTS_D1_SAFE_0 <- df5$PRED_LEVEL5_TREATMENTS_D1_SAFE_0
df5 <- df5[, !(grepl("_SAFE_0$", names(df5)) & names(df5) != "LEVEL5_TREATMENTS_D1_SAFE_0")]

# Joining ----
dfs <- list(df1, df2, df3, df4, df5)

df_final <- Reduce(
  function(x, y) {
    inner_join(
      x,
      y %>% select(label, matches("^LEVEL[0-9]+_TREATMENTS_D1_SAFE_0$")),
      by = "label"
    )
  },
  dfs
)

safe_cols <- grep("^LEVEL[0-9]+_TREATMENTS_D1_SAFE_0$", names(df_final), value = TRUE)

anchor_col <- safe_cols[1]
anchor_pos <- match(anchor_col, names(df_final))

tail_cols <- c("stratum")
tail_cols <- tail_cols[tail_cols %in% names(df_final)]

pre_cols <- setdiff(names(df_final)[seq_len(anchor_pos - 1)], tail_cols)
mid_cols <- safe_cols
post_cols <- setdiff(names(df_final), c(pre_cols, mid_cols, tail_cols))

df_final <- df_final %>%
  select(all_of(pre_cols), all_of(mid_cols), all_of(post_cols), all_of(tail_cols))

# Saving data with predicted treatments ----
saveRDS(
  df_final,
  file = file.path(save_dir, "treatment_predicted_train.rds")
)