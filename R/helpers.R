`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

as_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  val <- tolower(trimws(as.character(x[[1]])))
  if (!nzchar(val)) return(default)
  val %in% c("1", "true", "t", "yes", "y", "on")
}

parse_cli_options <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- list()
  if (!length(args)) return(opts)
  for (arg in args) {
    if (!startsWith(arg, "--")) next
    eq <- regexpr("=", arg, fixed = TRUE)
    if (eq <= 0) next
    key <- substring(arg, 3, eq - 1)
    val <- substring(arg, eq + 1)
    if (nzchar(key)) opts[[key]] <- val
  }
  opts
}

get_cli_option <- function(opts, key, default = NULL) {
  val <- opts[[key]]
  if (is.null(val) || !nzchar(trimws(as.character(val)))) return(default)
  val
}

