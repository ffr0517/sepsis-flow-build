# Global settings ----
source(here::here("R", "bootstrap.R"))

input_dir <- PATHS$ensemble_workspace
table_dir <- PATHS$tables_analysis
figure_dir <- PATHS$figures_analysis
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Load variable importance data ----
files <- list.files(
  input_dir,
  pattern = "_var_imp\\.rds$",
  full.names = TRUE
)

# Building data ----
build_long_from_file <- function(path) {
  outcome <- sub("_var_imp\\.rds$", "", basename(path))
  treatment <- str_extract(outcome, "^LEVEL[0-9]+")
  day <- str_extract(outcome, "D[0-9]+")
  if (is.na(day)) day <- "D1"
  
  varimp_list <- readRDS(path)
  nms <- names(varimp_list)
  if (is.null(nms) || all(is.na(nms)) || all(nms == "")) {
    nms <- paste0("df_", seq_along(varimp_list), "_varimp")
  }
  
  map2_dfr(varimp_list, nms, function(df, nm) {
    idx <- str_extract(nm, "[0-9]+")
    if (is.na(idx)) idx <- as.character(match(nm, nms))
    
    df %>%
      transmute(
        row_id = paste0(treatment, "_", day, "_df_", idx),
        TREATMENT = treatment,
        Variable,
        Importance
      )
  })
}

long_df <- map_dfr(files, build_long_from_file)

wide_tbl <- long_df %>%
  group_by(row_id, TREATMENT, Variable) %>%
  summarise(Importance = mean(Importance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Variable,
    values_from = Importance,
    values_fill = 0
  ) %>%
  arrange(TREATMENT, row_id)

wide_df <- as.data.frame(wide_tbl)
rownames(wide_df) <- wide_df$row_id
wide_df$row_id <- NULL

zv_cols <- wide_df %>%
  select(-TREATMENT) %>%
  summarise(across(everything(), ~ {
    x <- .x
    x <- x[is.finite(x)]
    if (length(x) <= 1) TRUE else var(x) == 0
  })) %>%
  pivot_longer(everything(), names_to = "col", values_to = "is_zv") %>%
  filter(is_zv) %>%
  pull(col)

wide_df <- wide_df %>%
  select(-all_of(zv_cols))

# PCA (day 1) ----
pca_mat <- wide_df %>%
  select(-TREATMENT) %>%
  as.matrix()

pca_mat[!is.finite(pca_mat)] <- 0

pca_res <- prcomp(pca_mat, center = TRUE, scale. = TRUE)

pca_scores <- as.data.frame(pca_res$x) %>%
  mutate(row_id = rownames(pca_res$x))

pca_loadings <- as.data.frame(pca_res$rotation) %>%
  mutate(Variable = rownames(pca_res$rotation))

pca_var <- pca_res$sdev^2
pca_var_explained <- pca_var / sum(pca_var)
pca_var_df <- data.frame(
  PC = paste0("PC", seq_along(pca_var_explained)),
  var_explained = pca_var_explained,
  cum_var_explained = cumsum(pca_var_explained)
)

var_labels <- c(
  "age.months" = "Age",
  "rr.all" = "Respiratory rate",
  "oxy.ra" = "SpO2",
  "wfaz" = "Weight-for-age Z",
  "sex" = "Sex",
  "adm.recent" = "Recent admission",
  "cidysymp" = "Symptom duration",
  "not.alert" = "AVPU < A",
  "hr.all" = "Heart rate",
  "envhtemp" = "Axillary temperature",
  "crt.long" = "Capillary refil time >2s"
)

label_var <- function(v) {
  unname(ifelse(v %in% names(var_labels), var_labels[v], v))
}

fmt_loading <- function(x) formatC(x, digits = 3, format = "f")

top_loadings_str <- function(load_vec, which_side = c("pos", "neg"), n = 2) {
  which_side <- match.arg(which_side)
  
  if (which_side == "pos") {
    v <- load_vec[load_vec > 0]
    v <- sort(v, decreasing = TRUE)
  } else {
    v <- load_vec[load_vec < 0]
    v <- sort(v, decreasing = FALSE)
  }
  
  if (length(v) == 0) return("")
  
  v <- head(v, n)
  nm <- label_var(names(v))
  paste0(nm, " (", fmt_loading(unname(v)), ")", collapse = ", ")
}

var_pct <- 100 * (pca_res$sdev^2 / sum(pca_res$sdev^2))
names(var_pct) <- colnames(pca_res$rotation)

pc_table <- tibble::tibble(
  `Principal component` = colnames(pca_res$rotation),
  `Variance explained (%)` = round(var_pct[colnames(pca_res$rotation)], 2),
  `Dominant positive loadings` = purrr::map_chr(
    colnames(pca_res$rotation),
    ~ top_loadings_str(pca_res$rotation[, .x], "pos", n = 2)
  ),
  `Dominant negative loadings` = purrr::map_chr(
    colnames(pca_res$rotation),
    ~ top_loadings_str(pca_res$rotation[, .x], "neg", n = 2)
  )
)

pca_scores_df <- as.data.frame(pca_res$x) %>%
  mutate(row_id = rownames(pca_res$x)) %>%
  left_join(
    wide_df %>%
      mutate(row_id = rownames(wide_df)) %>%
      select(row_id, TREATMENT),
    by = "row_id"
  )

# Median PC1–PC3 by treatment (Day 1)
pc_medians_by_treatment <- pca_scores_df %>%
  group_by(TREATMENT) %>%
  summarise(
    `Median PC1` = median(PC1, na.rm = TRUE),
    `Median PC2` = median(PC2, na.rm = TRUE),
    `Median PC3` = median(PC3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(`Treatment (Day 1)` = TREATMENT) %>%
  select(`Treatment (Day 1)`, `Median PC1`, `Median PC2`, `Median PC3`) %>%
  arrange(`Treatment (Day 1)`)

treatment_labels <- c(
  "LEVEL1" = "Mechanical ventilation, inotropes, or renal replacement therapy",
  "LEVEL2" = "CPAP or IV fluid bolus",
  "LEVEL3" = "ICU admission with clinical reason",
  "LEVEL4" = "O2 via face or nasal cannula",
  "LEVEL5" = "Non-bolused IV fluids"
)

pc_medians_by_treatment <- pc_medians_by_treatment %>%
  mutate(
    `Treatment (Day 1)` = dplyr::recode(`Treatment (Day 1)`, !!!treatment_labels)
  )

safe_lim <- function(x, pad = 1) {
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r))) return(c(-pad, pad))
  if (r[1] == r[2]) return(c(r[1] - pad, r[2] + pad))
  r
}

top_n <- 2

biplot_scores <- as.data.frame(pca_res$x[, c("PC1", "PC2"), drop = FALSE]) %>%
  mutate(row_id = rownames(pca_res$x)) %>%
  left_join(
    wide_df %>%
      mutate(row_id = rownames(wide_df)) %>%
      select(row_id, TREATMENT),
    by = "row_id"
  ) %>%
  mutate(
    TREATMENT = factor(
      TREATMENT,
      levels = names(treatment_labels),
      labels = unname(treatment_labels)
    )
  )

rot <- as.data.frame(pca_res$rotation[, c("PC1", "PC2"), drop = FALSE]) %>%
  mutate(Variable = rownames(pca_res$rotation))

key_vars <- rot %>%
  pivot_longer(cols = c("PC1", "PC2"), names_to = "PC", values_to = "loading") %>%
  group_by(PC) %>%
  summarise(
    key = c(
      Variable[order(loading, decreasing = TRUE)][seq_len(min(top_n, sum(loading > 0)))],
      Variable[order(loading, decreasing = FALSE)][seq_len(min(top_n, sum(loading < 0)))]
    ),
    .groups = "drop"
  ) %>%
  unnest(key) %>%
  distinct(key) %>%
  pull(key)

biplot_loadings <- rot %>%
  mutate(
    Variable_label = ifelse(Variable %in% names(var_labels), var_labels[Variable], Variable),
    is_key = Variable %in% key_vars
  )

score_max <- apply(abs(biplot_scores[, c("PC1", "PC2")]), 2, max, na.rm = TRUE)
loading_max <- apply(abs(biplot_loadings[, c("PC1", "PC2")]), 2, max, na.rm = TRUE)
loading_max <- pmax(loading_max, 1e-12)

arrow_mult <- 0.8 * min(score_max / loading_max)
if (!is.finite(arrow_mult) || arrow_mult <= 0) arrow_mult <- 1

biplot_loadings <- biplot_loadings %>%
  mutate(PC1 = PC1 * arrow_mult, PC2 = PC2 * arrow_mult)

xlim <- safe_lim(biplot_scores$PC1, pad = 1)
ylim <- safe_lim(biplot_scores$PC2, pad = 1)

p <- ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.3, alpha = 0.4) +
  geom_vline(xintercept = 0, linewidth = 0.3, alpha = 0.4) +
  geom_point(
    data = biplot_scores,
    aes(x = PC1, y = PC2, colour = TREATMENT),
    shape = 19,
    size = 2,
    alpha = 0.75
  ) +
  geom_segment(
    data = biplot_loadings %>% filter(is_key),
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = arrow(length = grid::unit(0.15, "inches")),
    colour = "black",
    linewidth = 0.6,
    alpha = 0.95
  ) +
  labs(
    title = "Variable-Importance Structure Across Day-1 Treatment Models (PCA)",
    subtitle = "Points represent ensemble models, coloured by the Day-1 treatment it was trained to predict.",
    x = "PC1",
    y = "PC2",
    colour = "Day 1 Treatment"
  ) +
  coord_cartesian(xlim = xlim, ylim = ylim) +
  theme_classic() +
  theme(
    plot.title = element_text(colour = "black", size = 17.5, face = "bold"),
    plot.subtitle = element_text(colour = "grey30", size = 12.5, face = "italic"),
    axis.text = element_text(size = 15.5, colour = "black"),
    legend.position = "right"
  )

# Label ONLY the dominant arrows, with a background
if (requireNamespace("ggrepel", quietly = TRUE)) {
  p <- p +
    ggrepel::geom_label_repel(
      data = biplot_loadings %>% filter(is_key),
      aes(x = PC1, y = PC2, label = Variable_label),
      colour = "black",
      fill = "white",
      alpha = 0.9,
      label.size = 0.2,
      size = 4,
      max.overlaps = Inf
    )
} else {
  p <- p +
    geom_label(
      data = biplot_loadings %>% filter(is_key),
      aes(x = PC1, y = PC2, label = Variable_label),
      colour = "black",
      fill = "white",
      alpha = 0.9,
      label.size = 0.2,
      size = 4
    )
}


out_path <- file.path(figure_dir, "pca_d1_biplot.pdf")
ggsave(out_path, p, width = 9, height = 6, device = "pdf", useDingbats = FALSE)

saveRDS(pc_medians_by_treatment, file = file.path(table_dir, "d1_pc_medians_by_treatment.rds"))
saveRDS(pc_table, file = file.path(table_dir, "d1_pc_table.rds"))
