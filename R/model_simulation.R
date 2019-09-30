
#' Simulate a [`daps`] object.
#'
#' Simulates data from `t.start` to `t.stop` for each subject using the
#' metastate model provided in [add_metastate_model()] and the trained model
#' fits created during [train()].
#'
#'
#'
#' @param daps A [`daps`]-class object to simulate. Must have a
#'   `daps_trained_fits` component and a `daps_metastate_model` component, added
#'   with [train()] and [add_metastate_model()], respectively.
#'
#' @param static A data frame with a unique numeric column `id`.
#' @param longitudinal A data frame with a numeric `id` column and an
#'   [integerish][rlang::is_integerish] `t` column.
#' @param s A nonzero integer indicating the number of simulations to perform.
#' @param t.start,t.stop Each should be a nonzero integer corresponding to the
#'   time point (i.e., the value of `t` as in the `longitudinal` data set) when
#'   simulation should begin and end. These can both be higher than the highest
#'   value(s) of the `t` column in `longitudinal`. `t.stop` must not be smaller
#'   than `t.start`.
#' @param start.state A character string indicated the initial state of the
#'   subjects (i.e., at `t = t.start - 1`). Must be one of the states specified
#'   in [add_metastate_model()].
#'
#' @return A list of [`tibble`][tibble::tibble]`s` with length `s` with
#'   simulated data.
#'
#' @import rlang
#' @export
simulate <- function(daps, 
                     static = NULL, 
                     longitudinal = NULL, 
                     s = 1,
                     
                     # here for testing purposes for now:
                     t.start, 
                     t.stop, 
                     start.state) {
  
  data_split <- setup_sim_data(static, longitudinal, t.start, t.stop)
  
  sim_row_env <- new_environment(list(.__daps_sim_row__. = NA_integer_))
  sim_row_fn <-
    new_function(
      args = list(),
      body = quote(.__daps_sim_row__.),
      env = sim_row_env
    )
  
  transitions <-
    lapply(
      daps$metastate_model$transitions,
      prep_transition_env,
      sub_key = attr(daps$trained_fits, "sub_key"),
      sim_row_fn = sim_row_fn
    )
  
  fits <- daps$trained_fits
  fits$model <- fits$model %>% lapply(prep_model_env, sim_row_fn = sim_row_fn)
  
  lapply(
    seq_len(s),
    function(s) {
      purrr::map_dfr(
        data_split,
        simulate_subject,
        mm = daps$metastate_model,
        transitions = transitions,
        fits = fits,
        s = s,
        t.start = t.start,
        t.stop = t.stop,
        start.state = start.state,
        sim_row_env = sim_row_env
      )
    }
  )
}




simulate_subject <- function(data,
                             mm,
                             transitions,
                             fits,
                             s,
                             t.start,
                             t.stop,
                             start.state,
                             sim_row_env) {
  
  row.start <- match(t.start, data$t)
  row.stop <- match(t.stop, data$t)
  
  data$state[row.start - 1L] <- start.state
  
  for (.__daps_sim_row__. in row.start:row.stop) {
    
    sim_row_env$.__daps_sim_row__. <- .__daps_sim_row__.
    
    transition_call <-
      transitions[[match(data$state[.__daps_sim_row__. - 1L], mm$state)]]
    
    data$state[.__daps_sim_row__.] <- state <- eval_tidy(transition_call, data)
    
    nodes <- mm$nodes[[match(state, mm$state)]]
    
    for (var in nodes) {
      
      if (is.na(data[[var]][.__daps_sim_row__.])) {
        data[[var]][.__daps_sim_row__.] <-
          get_prediction(fits, var, nodes, data, s)
      }
      
    }
    
  }
  
  data %>%
    dplyr::as_tibble() %>%
    dplyr::select("id", "t", "state", dplyr::everything())
}



setup_sim_data <- function(static, longitudinal, t.start, t.stop) {
  
  static$id <- validate_data(static, type = "static")
  
  longitudinal[c("id", "t")] <- validate_data(longitudinal, "longitudinal")
  
  longitudinal <- longitudinal %>%
    tidyr::complete(.data$id, t = t.start:t.stop) %>% 
    dplyr::arrange(.data$id, .data$t)
  
  dplyr::full_join(static, longitudinal, by = "id") %>%
    dplyr::mutate(state = NA_character_) %>%
    dplyr::group_split(.data$id) %>%
    lapply(as.list)
}



prep_transition_env <- function(transition_quo, sub_key, sim_row_fn) {
  
  expr <- do_call_substitute(expr = quo_get_expr(transition_quo), env = sub_key)

  env <-
    new_environment(data = rowwise_fns, parent = quo_get_env(transition_quo))
  env_bind_active(.env = env, .__daps_sim_row__. = sim_row_fn)
  
  new_quosure(expr = expr, env = env)

  # transition_quo %>% 
  #   quo_get_expr() %>% 
  #   do_call_substitute(env = sub_key) %>% 
  #   quo_set_expr(quo = transition_quo)
}



prep_model_env <- function(model, ...) {
  UseMethod("prep_model_env")
}



#' @export
prep_model_env.daps_trained_lm <- function(model, sim_row_fn, ...) {
  
  env <- new_environment(data = rowwise_fns, parent = environment(model$terms))
  env_bind_active(.env = env, .__daps_sim_row__. = sim_row_fn)
  
  environment(model$terms) <- env
  
  model
}



#' @export
prep_model_env.daps_deterministic_expr <- function(model, sim_row_fn, ...) {
  
  env <- new_environment(data = rowwise_fns, parent = quo_get_env(model))
  
  env_bind_active(.env = env, .__daps_sim_row__. = sim_row_fn)
  
  quo_set_env(quo = model, env = env)
}




get_prediction <- function(fits, var, nodes, data, s) {
  
  candidate_rows <- which(fits$var == var)
  
  for (row in candidate_rows) {
    
    if (all(fits$predictors[[row]] %in% nodes)) {
      
      predicted_value <-
        predict(
          fits$model[[row]],
          data = data,
          s = s
        )
      
      if (!is.na(predicted_value)) {
        return(predicted_value)
      }
      
    }
  }
  
  # stop(
  #   "No model able to predict ", var, " at time ", evalq(t, mask),
  #   call. = FALSE
  # )
  NA
}



#' @export
predict.daps_trained_lm <- function(model, data, s, ...) {
  
  predict.lm(
    model,
    newdata = data,
    ...
  )[[1L]]
  
}




#' @export
predict.daps_deterministic_expr <- function(model, data, ...) {
  eval_tidy(model, data)
}






#' @export
predict.daps_trained_multinom <- function(model, data, s, ...) {
  
  terms <- model$terms
  
  newdata <- 
    terms %>%
    attr("predvars2") %>%
    eval_tidy(mask, environment(terms)) %>% 
    list() %>% 
    dplyr::tibble(data = .)
  
  NextMethod(newdata = newdata)
}




# predict.daps_trained_fit_glm <- function(model, data, s) {
#   newdata <- eval_tidy(model$predvars, data)
#   # Account for intercept-free models
#   model$linkinv(newdata %*% model$coef[s, ])
# }








# scalarize_static_vars <- function(x, static) {
#   lapply(
#     x,
#     purrr::map_at,
#     .at = dplyr::vars(colnames(static), -"id"),
#     .f = `[`,
#     1L
#   )
# }
