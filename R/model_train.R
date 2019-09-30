#' Train existing individual variable models.
#'
#' Runs the model calls that were added during individual model specification
#' (i.e., in [add_models()]).
#'
#' The arguments `static` and `longitudinal` are validated and
#' [dplyr::full_join()]ed on the `id` column. This data set is inserted as the
#' `data` argument in each of the individual model calls, and each one is run.
#' Thus, these data should be just like the data that will ultimately be used
#' for simulation (i.e., in [simulate()]).
#'
#' @param daps A [`daps`]-class object to add trained individual model fits to.
#'
#' @param static A data frame with a unique numeric column `id`.
#' @param longitudinal A data frame with a numeric `id` column and an
#'   [integerish][rlang::is_integerish] `t` column.
#'
#' @return The input `daps` with the added component `$trained_fits`, an object
#'   of class `daps_trained_fits`.
#'
#' @import rlang
#' @export
train <- function(daps, static, longitudinal) {
  
  static["id"] <- validate_data(static, "static")
  
  longitudinal[c("id", "t")] <- validate_data(longitudinal, "longitudinal")
  
  joined_data <- dplyr::full_join(static, longitudinal, by = "id")
  
  all_vars <- colnames(joined_data)
  
  sub_key <- 
    all_vars %>% 
    # `[`(. != "t") %>% 
    stats::setNames(nm = .) %>% 
    lapply(function(x) expr((!!sym(x))[.__daps_sim_row__.]))
  
  daps$trained_fits <-
    daps$model_table %>% 
    purrr::pmap_dfr(
      function(var, predictors, model_call) {
        
        trained_model <-
          train_model(
            model_call = model_call, 
            sub_key = sub_key,
            data = joined_data 
          )
        
        validated_predictors <- intersect(all_vars, predictors)
        
        dplyr::tibble(
          var = var, 
          predictors = list(validated_predictors),
          model = list(trained_model)
        )
      }
    ) %>% 
    structure(
      sub_key = sub_key,
      class = c("daps_trained_fits", class(.))
    )
  
  daps
}




train_model <- function(model_call, ...) {
  UseMethod("train_model")
}



#' @export
train_model.daps_nonfitter_call <- function(model_call, sub_key, ...) {
  
  model_call %>% 
    do_call_substitute(sub_key) %>% 
    structure(class = c("daps_deterministic_expr", class(.)))
  
}


#' @export
train_model.daps_fitter_call <- function(model_call, sub_key, data) {
  
  model_call_env <- quo_get_env(model_call)
  
  fitter_fn <- call_fn(model_call)
  
  fitter_fn_fmls <- fn_fmls_names(fitter_fn)
  
  if (any(fitter_fn_fmls == "formula")) {
    
    model_standardised <- call_standardise(model_call)
    
    call_args <- call_args(model_standardised)
    
    formula <-
      call_args[["formula"]] %>% 
      eval(model_call_env) %>%
      structure(class = c("daps_formula", class(.)))
    
    env_dependent_arg_names <- env_dependent_arg_names(fitter_fn, call_args)
    
    env_dependent_args <- call_args[env_dependent_arg_names]
    
    other_call_mods <-
      lapply(env_dependent_args, call2, .fn = "col_by_id")
    
    model <-
      model_standardised %>% 
      call_modify(formula = formula, data = data, !!!other_call_mods) %>% 
      eval_tidy(data)
    
    env_dependent_args <- env_dependent_args %>%
      lapply(do_call_substitute, env = sub_key)
    
    model$call <- model$call %>%
      call_modify(data = zap(), !!!env_dependent_args)
    
  } else {
    formula <-
      call_args(model_call)[[1L]] %>% 
      eval(data, model_call_env) %>% 
      structure(class = c("daps_formula", class(.)))
    
    mf <- stats::model.frame(formula, data = data)
    
    x <- stats::model.matrix(formula, data = data)
    y <- mf[[1L]]
    
    predvars <-
      mf %>%
      attr("terms") %>%
      stats::delete.response() %>% 
      attr("predvars") %>%
      do_call_substitute(sub_key)
    
    model <-
      model_call %>% 
      call_modify(formula = zap(), x = x, y = y) %>% 
      eval_tidy(data)
  }
  
  new_trained_fit(model, sub_key = sub_key)
}



new_trained_fit <- function(x, ...) {
  UseMethod("new_trained_fit")
}


#' @export
new_trained_fit.lm <- function(x, sub_key, ...) {
  
  attr(x$terms, "variables") <-
    x$terms %>% 
    attr("variables") %>% 
    call_args() %>%
    # lapply(do_call_substitute, env = sub_key) %>%
    {call2("list", !!!.)}
  
  attr(x$terms, "predvars") <-
    x$terms %>% 
    attr("predvars") %>% 
    call_args() %>%
    lapply(do_call_substitute, env = sub_key) %>%
    {call2("list", !!!.)}
  
  class(x) <- c("daps_trained_lm", class(x))
  
  x
}




env_dependent_arg_names <- function(fitter_fn, call_args) {
  intersect(
    x = names(call_args),
    y = 
      if (identical(fitter_fn, stats::lm)) {
        c("weights", "subset", "offset")
      } else if (identical(fitter_fn, stats::glm)) {
        c("weights", "subset", "offset", "etastart", "mustart")
      }
  )
}



#' @export
new_trained_fit.multinom <- function(x, sub_key, ...) {
  
  attr(x$terms, "predvars2") <-
    x$terms %>% 
    stats::delete.response() %>% 
    attr("predvars") %>% 
    call_args() %>%
    lapply(do_call_substitute, env = sub_key) %>%
    {call2("list", !!!.)}
  
  attr(x$terms, "predvars") <- quote(data[[1L]])
  
  class(x) <- c("daps_trained_multinom", class(x))
  
  x
}

#' @export
terms.daps_formula <- function(...) {
  
  terms <- stats::terms.formula(...)
  
  variables_attr_args <- terms %>% attr("variables") %>% call_args()
  
  new_variables_attr <- call2("variables_by_id", !!!variables_attr_args)
  
  structure(
    terms,
    variables = new_variables_attr
  )
}



variables_by_id <- function(...) {
  
  list_call <- call2("list", !!!exprs(...))
  
  caller_env <- caller_env()
  
  caller_env %>% 
    split(.$id) %>% 
    lapply(eval, expr = list_call) %>% 
    purrr::reduce(function(x, y) purrr::list_merge(x, !!!y))
}


#' @export
split.environment <- function(x, f, drop = FALSE, ...) {
  x %>% 
    as.list() %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    split.data.frame(f = f, drop = drop, ...) %>% 
    lapply(as_environment, parent = parent.env(x))
}



col_by_id <- function(expr) {
  
  expr <- enexpr(expr)
  
  caller_env <- caller_env()
  
  caller_env %>% 
    split(.$id) %>% 
    lapply(eval, expr = expr) %>% 
    purrr::flatten_dbl()
}


