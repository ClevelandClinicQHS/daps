do_call_substitute <- function(expr, env, ...) {
  UseMethod("do_call_substitute")
}

#' @export
do_call_substitute.default <- function(expr, env, ...) {
  do.call(substitute, list(expr, env))
}

#' @export
do_call_substitute.quosure <- function(expr, env, ...) {
  quo_set_expr(
    quo = expr,
    expr = do.call(substitute, list(quo_get_expr(expr), env))
  )
}



data_mask <- function(data, rowwise_env) {
  new_data_mask(
    bottom = as_environment(data, parent = rowwise_env),
    top = rowwise_env
  )
}



#' @export
#' @importFrom magrittr %>%
magrittr::`%>%`

#' @export
#' @importFrom dplyr lag
dplyr::lag


#' @export
#' @importFrom dplyr case_when
dplyr::case_when



substitute_parents <- function(expr, env = caller_env()) {
  
  if (!is_environment(env)) {
    stop("env must be an actual environment")
  }
  
  expr <- substitute(expr)
  
  env <-
    expr %>% 
    all.names(unique = TRUE) %>% 
    mget(envir = env, ifnotfound = syms(.), inherits = TRUE)
  
  eval(substitute(substitute(expr, env)))
}
