
make_predict_expr <- function(x, sub_key, sim_row_fn) {
  UseMethod("make_predict_expr")
}


#' @export
make_predict_expr.daps_nonfitter_quo <- function(x, sub_key, sim_row_fn) {
  
  expr <- pryr::substitute_q(quo_get_expr(x), env = sub_key)

  env <- env(quo_get_env(x), !!!rowwise_fns)
  env_bind_active(env, .__daps_sim_row__. = sim_row_fn)
  
  expr(eval_tidy(!!new_quosure(expr = expr, env = env), data))
}



#' @importFrom stats predict.lm
#' @export
make_predict_expr.lm <- function(x, sub_key, sim_row_fn) {
  
  qc <- get_predict_components(x, sub_key, sim_row_fn)
  
  expr({
    data_row <- eval_tidy(!!qc$data_row_quo, data)
    if (anyNA(data_row)) NA else predict.lm(!!qc$model, data_row)[[1L]]
  })
}



#' @importFrom stats predict.glm
#' @export
make_predict_expr.glm <- function(x, sub_key, sim_row_fn) {
  
  qc <- get_predict_components(x, sub_key, sim_row_fn)
  
  expr({
    data_row <- eval_tidy(!!qc$data_row_quo, data)
    if (anyNA(data_row)) NA else
      as.logical(
        round(
          predict.glm(
            !!qc$model, 
            data_row,
            type = "response"
          )[[1L]]
        )
      )
  })
}




#' @importFrom stats predict
#' @export
make_predict_expr.multinom <- function(x, sub_key, sim_row_fn) {
  
  qc <- get_predict_components(x, sub_key, sim_row_fn)
  
  expr({
    data_row <- eval_tidy(!!qc$data_row_quo, data)
    if (anyNA(data_row)) NA else as.character(predict(!!qc$model, data_row))
  })
}


#' @importFrom stats predict
#' @export
make_predict_expr.clm <- function(x, sub_key, sim_row_fn) {
  
  qc <- get_predict_components(x, sub_key, sim_row_fn)
  
  expr({
    data_row <- eval_tidy(!!qc$data_row_quo, data)
    if (anyNA(data_row)) NA else
      as.character(predict(!!qc$model, data_row, type = "class")$fit)
  })
}




#' @importFrom dplyr tibble
get_predict_components <- function(x, sub_key, sim_row_fn) {
  
  variables_attr_args <- attr(x$terms, "variables") %>% call_args()
  
  attr(x$terms, "variables") <- call2("list", !!!variables_attr_args)
  
  daps_data_row_names <- as.character(variables_attr_args)
  
  data_row_quo_expr <-
    attr(x$terms, "predvars") %>%
    call_args() %>%
    `[`(-1L) %>% 
    set_names(daps_data_row_names[-1L]) %>% 
    lapply(pryr::substitute_q, env = sub_key) %>%
    {call2("tibble", !!!.)}
  
  attr(x$terms, "predvars") <- call2("list", !!!syms(daps_data_row_names))

  data_row_quo_env <- env(environment(x$terms), !!!rowwise_fns)
  env_bind_active(data_row_quo_env, .__daps_sim_row__. = sim_row_fn)
  
  data_row_quo <- new_quosure(expr = data_row_quo_expr, env = data_row_quo_env)
  
  list(model = x, data_row_quo = data_row_quo)
}
