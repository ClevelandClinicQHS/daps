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
  static <- validate_data(static, data = "static", action = "train")
  
  longitudinal <-
    validate_data(longitudinal, data = "longitudinal", action = "train")
  
  joined_data <- dplyr::full_join(static, longitudinal, by = "id")
  
  all_vars <- colnames(joined_data)
  
  # sub_key <- 
  #   all_vars %>% 
  #   # `[`(. != "t") %>% 
  #   stats::setNames(nm = .) %>% 
  #   lapply(function(x) expr((!!sym(x))[.__daps_sim_row__.]))
  
  daps$trained_fits <-
    daps$model_table %>% 
    purrr::pmap_dfr(
      function(var, predictors, model_quo, ...) {
        
        validated_predictors <- intersect(all_vars, predictors)
        
        trained_model <-
          train_model(
            model_quo = model_quo, 
            data = joined_data 
          )
        
        dplyr::tibble(
          var = var, 
          predictors = list(validated_predictors),
          model = list(trained_model)
        )
      }
    ) %>% 
    structure(class = c("daps_trained_fits", class(.)))
  
  daps
}




train_model <- function(model_quo, ...) {
  UseMethod("train_model")
}



#' @export
train_model.daps_nonfitter_quo <- function(model_quo, ...) {
  model_quo
}


#' @export
train_model.daps_fitter_quo <- function(model_quo, data) {
  
  model_quo_env <- quo_get_env(model_quo)

  fitter_fn <- call_fn(model_quo)

  fitter_fn_fmls <- fn_fmls_names(fitter_fn)

  if (any(fitter_fn_fmls == "formula")) {

    model_standardised <- call_standardise(model_quo)

    quo_args <- call_args(model_standardised)

    formula <- quo_args[["formula"]] %>% eval(model_quo_env)
    
    class(formula) <- c("daps_formula", class(formula))
    
    environment(formula) <-
      env(
        environment(formula),
        .__daps_variables_by_id__. = .__daps_variables_by_id__.
      )

    # env_dependent_arg_names <- env_dependent_arg_names(fitter_fn, quo_args)
    # 
    # env_dependent_args <- quo_args[env_dependent_arg_names]
    # 
    # other_quo_mods <-
    #   lapply(env_dependent_args, call2, .fn = "col_by_id")

    model <-
      model_standardised %>%
      call_modify(formula = formula, data = data) %>%
      # call_modify(formula = formula, data = data, !!!other_call_mods) %>%
      eval_tidy(data)

    # env_dependent_args <- env_dependent_args %>%
    #   lapply(do_call_substitute, env = sub_key)
    # 
    # model$call <- model$call %>%
    #   call_modify(data = zap(), !!!env_dependent_args)

    # variables_attr_args <- attr(model$terms, "variables") %>% call_args()
    # 
    # attr(model$terms, "variables") <- call2("list", !!!variables_attr_args)
    # 
    # daps_data_row_names <- as.character(variables_attr_args)
    # 
    # attr(model, "daps_data_row_call") <-
    #   attr(model$terms, "predvars") %>%
    #   call_args() %>%
    #   setNames(daps_data_row_names) %>% 
    #   lapply(do_call_substitute, env = sub_key) %>%
    #   {call2("tibble", !!!.)}
    # 
    # attr(model$terms, "predvars") <- call2("list", !!!syms(daps_data_row_names))
    # 
    # class(model) <- class(model) %>% 
    #   c("daps_trained_model", paste0("daps_trained_", .[1L]), .)
    
    model
    
  } 
  # else {
  #   formula <-
  #     call_args(model_quo)[[1L]] %>%
  #     eval(data, model_quo_env) %>%
  #     structure(class = c("daps_formula", class(.)))
  # 
  #   mf <- stats::model.frame(formula, data = data)
  # 
  #   x <- stats::model.matrix(formula, data = data)
  #   y <- mf[[1L]]
  # 
  #   predvars <-
  #     mf %>%
  #     attr("terms") %>%
  #     stats::delete.response() %>%
  #     attr("predvars") %>%
  #     do_call_substitute(sub_key)
  # 
  #   model <-
  #     model_quo %>%
  #     call_modify(formula = zap(), x = x, y = y) %>%
  #     eval_tidy(data)
  # }
  # 
  # new_trained_fit(model, sub_key)
}



#' @export
terms.daps_formula <- function(x, ...) {
  
  terms <- NextMethod()
  
  attr(terms, "variables") <-
    call2(".__daps_variables_by_id__.", !!!call_args(attr(terms, "variables")))
  
  terms
}



.__daps_variables_by_id__. <- function(...) {
  
  caller_env <- caller_env()
  
  caller_env %>% 
    as.list() %>% 
    dplyr::as_tibble() %>% 
    dplyr::group_by(.data$id) %>% 
    dplyr::transmute(...) %>%
    dplyr::ungroup() %>% 
    dplyr::select(-"id") %>% 
    lapply(identity)
}
