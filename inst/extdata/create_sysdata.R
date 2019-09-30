
lag <- function(x, n = 1, default = NA, ...) {
  
  if (!rlang::is_scalar_integerish(n) || n < 0) {
    stop("n must be a nonnegative, scalar integer")
  }
  
  caller_env <- rlang::caller_env()
  
  if (rlang::env_get("t", env = caller_env, inherit = TRUE) <= n) {
    return(default)
  }
  
  expr <- rlang::enexpr(x)
  env  <- list(t = rlang::expr(t - !!n))
  
  lag_expr <- do_call_substitute(expr, env)
  
  eval(expr = lag_expr, envir = caller_env)
}

usethis::use_data(lag, internal = TRUE, overwrite = TRUE)