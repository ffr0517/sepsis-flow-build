ensure_dirs <- function(paths) {
  for (p in unlist(paths, use.names = FALSE)) {
    if (is.character(p) && length(p) == 1 && nzchar(p) && !dir.exists(p)) {
      dir.create(p, recursive = TRUE, showWarnings = FALSE)
    }
  }
}

path_from_root <- function(...) {
  here::here(...)
}

