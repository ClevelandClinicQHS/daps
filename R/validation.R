validate_data <- function(x, type) {
  
  if (!is.data.frame(x)) {
    stop(type, " must be a data frame")
  }
  
  if (is.null(id <- x$id) ||
      !is_integerish(id) ||
      any(is.na(id)) ||
      type == "static" && anyDuplicated(id)) {
    stop(
      type, ' must have an "id" column that contains ',
      if (type == "static") "unique, ",
      "non-missing, non-negative integers"
    )
  }
  
  if (type == "longitudinal") {
    
    if (is.null(t <- x$t) ||
        !is_integerish(t) ||
        any(is.na(t)) ||
        any(t < 0L) ||
        anyDuplicated(x[c("id", "t")])) {
      stop(
        'longitudinal must have "t" column that contains non-missing, ',
        "non-negative integers with no duplicates per id"
      )
    }
    
    list(id = as.integer(id), t = as.integer(t))
  } else {
    as.integer(id)
  }
}



validate_lhs <- function(x, original_input = x) {
  
  force(original_input)
  
  lhs <- f_lhs(x)
  
  if (!is_symbol(lhs)) {
    stop(
      "Lefthand side of the model formula:\n",
      as_label(original_input),
      'must be a single column name, not an object of type "', typeof(lhs), '".'
    )
  }
  
  as.character(lhs)
}


is_valid_char_vec <- function(x) {
  is.character(x) && length(x) > 0L && !any(is.na(x)) && !anyDuplicated(x)
}
