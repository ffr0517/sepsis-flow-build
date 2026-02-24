source(here::here("R", "bootstrap.R"))

manifest_path <- here::here("config", "pipeline_manifest.csv")
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)

expand_row <- function(row) {
  extras <- strsplit(row[["required_inputs"]], ";", fixed = TRUE)[[1]]
  extras <- trimws(extras)
  extras <- extras[nzchar(extras)]
  if (!length(extras) || !grepl("^scripts/", extras[1])) return(as.data.frame(row, stringsAsFactors = FALSE))

  scripts <- c(row[["script_path"]], extras)
  out <- lapply(seq_along(scripts), function(i) {
    r <- row
    r[["script_path"]] <- scripts[[i]]
    if (i == 1) r[["required_inputs"]] <- ""
    as.data.frame(r, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

manifest_expanded <- do.call(rbind, lapply(seq_len(nrow(manifest)), function(i) expand_row(manifest[i, ])))
manifest_expanded <- manifest_expanded[order(manifest_expanded$order, manifest_expanded$script_path), ]

for (i in seq_len(nrow(manifest_expanded))) {
  row <- manifest_expanded[i, ]
  script_rel <- row$script_path
  script_abs <- here::here(script_rel)
  if (!file.exists(script_abs)) {
    stop("Pipeline script not found: ", script_rel)
  }
  message("[pipeline] Running ", row$stage_id, " -> ", script_rel)
  output <- system2("Rscript", c(script_abs, commandArgs(trailingOnly = TRUE)), stdout = TRUE, stderr = TRUE)
  cat(paste(output, collapse = "\n"), "\n")
  exit_code <- attr(output, "status")
  if (!is.null(exit_code) && exit_code != 0) {
    stop("Stage failed (", row$stage_id, "): ", script_rel, " [exit ", exit_code, "]")
  }
}
