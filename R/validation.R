
#' @importFrom dplyr arrange
#' @importFrom tidyr complete
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
    x <- x %>% complete(id = .env$long_ids) %>% arrange(.data$id)
    
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




validate_predictors <- function(x, model_id, ...) {
  UseMethod("validate_predictors")
}



#' @import rlang
#' @export
validate_predictors.daps_fitter_quo <- function(x, model_id, ...) {
  quo_get_expr(x)$formula[-2L] %>% 
    terms.formula() %>% 
    attr("variables") %>% 
    call_args() %>% 
    purrr::map_dfr(preds_from_call, env = quo_get_env(x), ...) %>% 
    tibble::add_column(model_id = model_id, .before = 1L)
}


#' @import rlang
#' @export
validate_predictors.daps_nonfitter_quo <- function(x, model_id, ...) {
  
  x %>% 
    quo_get_expr() %>% 
    preds_from_call(env = quo_get_env(x), ...) %>% 
    tibble::add_column(model_id = model_id, .before = 1L)
}


#' @importFrom tibble tibble
preds_from_call <- function(x,
                            temporal_vars,
                            static_vars,
                            env,
                            rowwise_fn_nms = ls(rowwise_fns, sorted = FALSE),
                            rowwise_fn_env = as.environment(rowwise_fns),
                            ...) {
  
  if (is.call(x)) {
    
    if (is_call(x, rowwise_fn_nms, ns = c("", "daps"))) {
      
      if (call_name(x) == "lag") {
        out <- validate_lag(x, temporal_vars, env, rowwise_fn_env)
      } else {
        out <- validate_slide(x, temporal_vars, env, rowwise_fn_env)
      }
      
    } else if (is_call(x[[1L]], rowwise_fn_nms, ns = c("", "daps"))) {
      stop(
        "\nThe special daps temporal functions (e.g., lag, slide_mean) do",
        "\nnot return functions, and cannot be called, as in:\n\n",
        as_label(x)
      )
    } else {
      out <-
        purrr::map_dfr(
          if (is.call(x[[1L]])) x else x[-1L],
          preds_from_call,
          temporal_vars = temporal_vars,
          static_vars = static_vars,
          env = env,
          rowwise_fn_nms = rowwise_fn_nms,
          rowwise_fn_env = rowwise_fn_env
        )
    }
  } else if (is_symbol(x, temporal_vars)) {
    out <-
      tibble(
        var = as_string(x),
        lag = 0L,
        call = exprs(.__daps_subset__.(!!x, .__daps_sim_row__.))
      )
  } else if (is_symbol(x, static_vars)) {
    out <- tibble(var = as_string(x), lag = NA_integer_, call = list(x))
  } else {
    out <- tibble(var = character(), lag = integer(), call = list())
  }
  
  out
}


validate_lag <- function(x, temporal_vars, env, rowwise_fn_env) {
  
  call <- call_standardise(x, rowwise_fn_env)
  
  if (!is_symbol(call$x, temporal_vars)) {
    stop(
      "\nThe x argument in calls to lag() must be the name of a single",
      "\nvariable in the in the longitudinal data set and must not contain any",
      "\nfunction calls. Offender:\n\n",
      as_label(x)
    )
  }
  
  var <- as_string(call$x)
  
  n <- eval(call$n, env, NULL)
  
  if (is.null(n)) {
    n <- 1L
  } else if (!is_scalar_integerish(n) || n < 1L) {
    stop(
      "\nn must be a positive integer in calls to lag():\n\n",
      as_label(x)
    )
  }
  
  default <- eval(call$default, env, NULL)
  
  if (is.null(default)) {
    default <- NA
  } else if (!is_scalar_atomic(default)) {
    stop(
      "\ndefault must be a single value:\n\n",
      as_label(x)
    )
  }
  
  call <-
    call_modify(
      call,
      x = expr(.__daps_subset__.(!!call$x, .__daps_sim_row__.)),
      n = n,
      default = default
    )
  
  tibble(var = var, lag = n, call = list(call))
}


validate_slide <- function(x, temporal_vars, env, rowwise_fn_env) {
  
  call <- call_standardise(x, rowwise_fn_env)
  
  if (!is_symbol(call$.x, temporal_vars)) {
    stop(
      "\nThe .x argument in calls to the slide_*() functions must be the name",
      "\nof a single variable in the in the longitudinal data set and must not",
      "\ncontain any function calls. Offender:\n\n",
      as_label(x)
    )
  }
  
  var <- as_string(call$.x)
  
  n <- eval(call$n, env, NULL)
  
  if (is.null(n)) {
    n <- Inf
  } else if (!(is_scalar_integerish(n) && n > 0L) &&
             !all.equal(Inf, n, check.attributes = FALSE)) {
    stop(
      "\nn must be a positive integer or Inf in calls to ",
      "special daps\n",
      "\ntemporal functions (e.g., slide_mean, slide_median):\n\n",
      as_label(x)
    )
  }
  
  call <-
    call_modify(
      call,
      .x = expr(.__daps_subset__.(!!call$.x, .__daps_sim_row__.)),
      n = n
    )
  
  tibble(var = var, lag = NA, call = list(call))
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
