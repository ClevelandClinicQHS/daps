
make_pred_expr <- function(x, env, sub_key) {
  UseMethod("make_pred_expr")
}


#' @export
make_pred_expr.daps_nonfitter_quo <- function(x, env, sub_key) {
  
  expr <- substitute_q2(quo_get_expr(x), env = sub_key)

  expr(eval_tidy(!!new_quosure(expr = expr, env = env), data))
}



#' @export
make_pred_expr.lm <- function(x, env, sub_key) {
  
  qc <- get_predict_components(x, env, sub_key)
  
  expr({
    data_row <- eval_tidy(!!qc$data_row_quo, data)
    if (anyNA(data_row)) NA else predict.lm(!!qc$model, data_row)[[1L]]
  })
}


#' @export
make_pred_expr.glm <- function(x, env, sub_key) {
  
  qc <- get_predict_components(x, env, sub_key)
  
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
make_pred_expr.multinom <- function(x, env, sub_key) {
  
  qc <- get_predict_components(x, env, sub_key)
  
  expr({
    data_row <- eval_tidy(!!qc$data_row_quo, data)
    if (anyNA(data_row)) NA else as.character(predict(!!qc$model, data_row))
  })
}


#' @importFrom stats predict
#' @export
make_pred_expr.clm <- function(x, env, sub_key) {
  
  qc <- get_predict_components(x, env, sub_key)
  
  expr({
    data_row <- eval_tidy(!!qc$data_row_quo, data)
    if (anyNA(data_row)) NA else
      as.character(predict(!!qc$model, data_row, type = "class")$fit)
  })
}




#' @importFrom tibble tibble
get_predict_components <- function(x, env, sub_key) {
  
  attr(x$terms, "variables")[[1L]] <- quote(list)
  
  daps_data_row_names <-
    attr(x$terms, "variables") %>%
    call_args() %>%
    as.character()
  
  data_row_quo_args <-
    x$terms %>% 
    delete.response() %>% 
    attr("predvars") %>%
    call_args() %>%
    set_names(daps_data_row_names[-1L]) %>% 
    lapply(substitute_q2, env = sub_key)
  
  data_row_quo_expr <- call2("tibble", !!!data_row_quo_args)
  
  attr(x$terms, "predvars") <- call2("list", !!!syms(daps_data_row_names))

  # data_row_quo_env <-
  #   environment(x$terms) %>% 
  #   append_sim_row_env(rowwise_fns, sim_row_fn)
  
  data_row_quo <- new_quosure(expr = data_row_quo_expr, env = env)
  
  list(model = x, data_row_quo = data_row_quo)
}
