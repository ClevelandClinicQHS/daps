
validate_data <- function(x, data, action, long_ids = NULL, metastates = NULL) {
  
  if (!is.data.frame(x)) {
    stop(data, " must be a data frame")
  }
  
  if (is.null(x$id) ||
      !is_integerish(x$id) ||
      anyNA(x$id) ||
      data == "static" && anyDuplicated(x$id)) {
    stop(
      data, ' must have an "id" column that contains ',
      if (data == "static") "unique, ",
      "non-missing, non-negative integers"
    )
  }
  
  x$id <- as.integer(x$id)
  
  if (data == "longitudinal") {
    
    if (is.null(x$t) ||
        !is_integerish(x$t) ||
        anyNA(x$t) ||
        any(x$t < 0L) ||
        anyDuplicated(x[c("id", "t")])) {
      stop(
        'longitudinal must have "t" column that contains non-missing, ',
        "non-negative integers with no duplicates per id"
      )
    }
    
    x$t <- as.integer(x$t)
    
    if (action == "simulate") {
      if (any(names(x) == "metastate")) {
        if (!all(is.na(x$metastate))) {
          stop(
            '"metastate" column in longitudinal must only have missing values.',
            "\nAlternatively, exclude it and it will be added automatically"
          )
        }
      } else {
        x <- x %>% tibble::add_column(metastate = NA, .after = "t")
      }
    }
    
  } else if (action == "simulate") {
    x <- x %>% tidyr::complete(id = .env$long_ids) %>% dplyr::arrange(.data$id)
    
    if (any(names(x) == "starting_metastate")) {
      
      x$starting_metastate <-
        dplyr::coalesce(x$starting_metastate, metastates[1L])
      
      if (!all(x$starting_metastate %in% metastates)) {
        stop(
          "\nThe following starting_metastates in static are not found",
          "\nin the metastate model:\n\n",
          paste0(setdiff(x$starting_metastate, metastates), collapse = "\n")
        )
      }
      
    } else {
      x <- x %>% tibble::add_column(starting_metastate = metastates[1L])
    }
  }
  
  x
}



# validate_start_metastate <- function(x, ids) {
#   
#   if (is_string(x)) {
#     out <- rep_along(along = ids, x)
#   } else if (is.data.frame(x)) {
#     if (ncol(x) != 2L || )
#   }
#   
#   out
# }



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
  is.character(x) && length(x) > 0L && !anyNA(x) && !anyDuplicated(x)
}



validate_from_to <- function(from, to, static, longitudinal, h = NULL) {
  
  from_is_last <- identical(from, "last")
  to_is_last <- identical(to, "last")
  
  if (from_is_last || to_is_last) {
    last <- 
      longitudinal %>%
      dplyr::group_by(.data$id) %>% 
      dplyr::summarise_at("t", max) %>% 
      dplyr::pull()
    if (from_is_last) {
      if (to_is_last) {
        to <- from <- last
      } else {
        from <- last + is.null(h)
      } 
    } else if (to_is_last) {
      to <- last
    }
  }
  
  if (!is_integerish(from) || !is_integerish(to)) {
    stop('\nfrom and to must each be an integer, an integer vector, or "last"')
  }
  
  length_from <- length(from)
  length_to <- length(to)
  nrow_static <- nrow(static)
  
  if (length_from == 1L) {
    from <- rep_len(from, nrow_static)
  } else if (length_from != nrow_static) {
    stop(
      '\nAll patients must have longitudinal data when specifying from = "last"'
    )
  }
  
  if (length_to == 1L) {
    to <- rep_len(to, nrow_static)
  } else if (length_to != nrow_static) {
    stop(
      '\nAll patients must have longitudinal data when specifying to = "last"'
    )
  }
  
  list(from = from, to = to)
}