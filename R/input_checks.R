require_files <- function(paths, stage_id = NULL) {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    prefix <- if (!is.null(stage_id)) paste0("[", stage_id, "] ") else ""
    stop(
      prefix,
      "Missing required file(s):\n- ",
      paste(missing, collapse = "\n- "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

log_stage <- function(stage_id, ..., .sep = "") {
  msg <- paste0(...)
  message("[", stage_id, "] ", msg, sep = .sep)
}

