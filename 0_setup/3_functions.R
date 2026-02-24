# Summarise each variable: return means/SDs for numeric vars and level proportions for categorical vars ----
summarise_var <- function(df, var) {
  x <- df[[var]]
  
  if (is.numeric(x)) {
    return(data.frame(
      variable = var,
      level = NA_character_,
      proportion = NA_real_,
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      n = sum(!is.na(x)),
      stringsAsFactors = FALSE
    ))
  }
  
  # Categorical branch
  tab <- table(x, useNA = "ifany")
  prop <- prop.table(tab)
  
  out <- data.frame(
    variable = var,
    level = names(prop),
    proportion = as.numeric(prop),
    mean = NA_real_,
    sd = NA_real_,
    n = NA_real_,
    stringsAsFactors = FALSE
  )
  
  return(out)
}