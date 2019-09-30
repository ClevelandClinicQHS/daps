#' Add individual variable models to a `daps` object.
#'
#' It is expected that multiple entries will apply to the same variable. They
#' should be entered in descending order of preference.
#'
#' @param daps The [`daps`]-class object to add individual variable models to.
#'
#' @param ... Individual variable models. This can by any combination of
#'   two-sided [`formula`][base::formula]`s` and calls to modeling functions.
#'
#'   The left side of each formula must specify a single variable that will
#'   appear in the data. The right side will be extracted and saved to be
#'   evaluated in the context of a row of the data during simulation (i.e., in
#'   [simulate()]) as needed.
#'
#'   Calls to modeling functions are not evaluated until model training (i.e.,
#'   in [train()]). They must be written without a `data` argument, as this
#'   argument will be filled in during training. The left side of the `formula`
#'   argument of modeling function calls must specify a single variable that
#'   will appear in the data.
#'
#'   Multiple formulas/model calls can specify the same variable. These should
#'   be listed in descending order of preference (which may usually also mean
#'   descending order of complexity).
#'
#'   These arguments are not evaluated but quoted.
#'
#'   Ignored if `daps_model_table` is not `NULL`.
#'
#' @param daps_model_table An object of class `daps_model_table` previously
#'   created. If not `NULL`, anything specified in `...` will be ignored.
#'
#' @return The input `daps` with the added component `$model_table`, an object
#'   of class `daps_model_table`.
#'
#' @export
add_models <- function(daps, ..., daps_model_table = NULL) {
  
  daps$model_table <-
    
    if (is.null(daps_model_table)) {
      
      enquos(...) %>%
        purrr::map_dfr(parse_model) %>% 
        structure(class = c("daps_model_table", class(.)))
      
    } else {
      
      if (!inherits(daps_model_table, "daps_model_table")) {
        stop('daps_model_table must be of class "daps_model_table"')
      }
      
      daps_model_table
      
    }
  
  daps
}



parse_model <- function(quo) {
  
  if (!quo_is_call(quo)) {
    stop(
      "The model:\n",
      as_label(quo),
      "\nmust be either a formula or a call to a modeling function."
    )
  }
  
  if (is_formula(quo_get_expr(quo))) {
    model_row_from_formula(quo)
  } else {
    model_row_from_call(quo)
  }
}



model_row_from_formula <- function(quo) {
  
  formula <- quo_get_expr(quo)
  
  var <- validate_lhs(formula, original_input = quo)
  
  model_call <- 
    formula %>% 
    f_rhs() %>% 
    quo_set_expr(quo = quo, expr = .) %>% 
    structure(class = c("daps_nonfitter_call", class(.)))
  
  predictors <- all.vars(model_call)
  
  dplyr::tibble(
    var = var,
    predictors = list(predictors), 
    model_call = list(model_call)
  )
}



model_row_from_call <- function(quo) {
  
  fitter_fn <- call_fn(quo)
  
  fitter_fn_fmls_names <- fn_fmls_names(fitter_fn)
  
  if (any(fitter_fn_fmls_names == "formula")) {
    
    quo <- call_standardise(quo)
    
    call_args <- call_args(quo)
    
    if (!is.null(call_args[["data"]])) {
      stop(
        "The call:\n",
        as_label(quo),
        "\nmay not use its data argument, since it will be overwritten ",
        "\nwith your temporal data once it is evaluated."
      )
    }
    
    formula <- call_args[["formula"]]
    
    if (!is_formula(formula, lhs = TRUE)) {
      stop(
        "The formula argument in the call:\n",
        as_label(quo),
        "\nmust be a two-sided formula."
      )
    }
  } else {
    
    first_arg <- call_args(quo)[1L]
    formula <- first_arg[[1L]]
    
    if (names(first_arg) != "" || !is_formula(formula, lhs = TRUE)) {
      stop(
        "The first argument in the call must not be named,",
        "\nand it must be a two-sided formula.",
        "\nIt will be processed and removed prior to evaluation.",
        "\nSee ?add_models"
      )
    }
  }
  
  var <- validate_lhs(formula, original_input = quo)
  
  predictors <- formula %>% f_rhs() %>% all.vars()
  
  model_call <- quo %>% structure(class = c("daps_fitter_call", class(.)))
  
  dplyr::tibble(
    var = var,
    predictors = list(predictors),
    model_call = list(model_call)
  )
}
