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
      
      enquos(..., .unquote_names = FALSE) %>%
        purrr::map_dfr(parse_model_spec) %>% 
        tibble::add_column(model_id = seq_len(nrow(.)), .before = 1L) %>% 
        structure(class = c("daps_model_table", class(.)))
      
    } else {
      
      if (!inherits(daps_model_table, "daps_model_table")) {
        stop('daps_model_table must be of class "daps_model_table"')
      }
      
      daps_model_table
      
    }
  
  daps
}


parse_model_spec <- function(quo) {
  
  if (!quo_is_call(quo, name = ":=", n = 2L, ns = "")) {
    stop(
      "The model:\n\n",
      as_label(quo),
      "\n\nmust be of the form:\n\n",
      "dynamic_var(t) := {model expression}",
      "\n\nor:\n\n",
      "static_var := {model expression}"
    )
  }
  
  formula <- eval_tidy(quo, list(`:=` = `~`))
  lhs <- f_lhs(formula)
  rhs <- f_rhs(formula)
  
  if (is_call(lhs, n = 1L, ns = "") && 
      all.equal(the_t <- call_args(lhs), syms("t"), check.attributes = FALSE) &&
      !is_named(the_t)) {
    
    var <- lhs[[1L]]
    dynamic <- TRUE
    
  } else if (is_symbol(lhs)) {
    
    var <- lhs
    dynamic <- FALSE
    
  } else {
    stop(
      "The lefthand side of the model equation:\n\n",
      as_label(quo),
      "\n\nmust be of the form:\n\n",
      "static_variable_name",
      "\n\nor:\n\n",
      "dynamic_variable_name(t)"
    )
  }
  
  if (is_call(rhs, name = "list", ns = "")) {
    
    model_exprs <- call_args(rhs)
    
    if (any(have_name(model_exprs))) {
      warning(
        "\nThe names in the model list:\n\n",
        as_label(rhs),
        "\n\nwill be ignored"
      )
    }

    purrr::map_dfr(model_exprs, parse_model, var, dynamic, quo)
    
  } else {
    parse_model(rhs, var, dynamic, quo)
  }
}



parse_model <- function(x, var, dynamic, quo) {
  
  is_fitter <-
    purrr::pmap_lgl(
      list(
        name = c("lm", "glm", "multinom", "clm"),
        ns = lapply(c("stats", "stats", "nnet", "ordinal"), c, "")
      ),
      is_call,
      x = x
    ) %>% 
    any()
  
  if (is_fitter) {
  
    if (call_name(x) == "multinom" &&
        !requireNamespace("nnet", quietly = TRUE)) {
      stop("\nPlease install the nnet package in order to use multinom()")
    } else if (call_name(x) == "clm" &&
               !requireNamespace("ordinal", quietly = TRUE)) {
      stop("\nPlease install the ordinal package in order to use clm()")
    }
    
    model_row_from_fitter(x, var, dynamic, quo)
  } else {
    model_row_from_nonfitter(x, var, dynamic, quo)
  }
}



model_row_from_fitter <- function(x, var, dynamic, quo) {
  
  expr_std <- call_standardise(x)
  
  formula <- expr_std$formula
  
  if (!is_formula(formula, lhs = FALSE)) {
    stop(
      '\nThe "formula" argument in the model:\n\n',
      as_label(x),
      "\n\nwithin the model specification:\n\n",
      as_label(quo),
      "\n\nmust be a formula with no lefthand side"
    )
  }
  
  if (!is.null(expr_std$data)) {
    stop(
      "The call:\n",
      as_label(x),
      '\nmay not use its "data" argument, since it will be overwritten ',
      "\nwith your temporal data once it is evaluated."
    )
  }
  
  # predictors <- all.vars(formula)
  
  f_lhs(formula) <- var
  expr_std$formula <- formula
  model_quo <- quo_set_expr(quo = quo, expr = expr_std)
  class(model_quo) <- c("daps_fitter_quo", class(model_quo))
  
  var <- as_string(var)
  
  tibble(
    var = var,
    dynamic = dynamic,
    # predictors = list(predictors),
    model_quo = list(model_quo)
  )
}



model_row_from_nonfitter <- function(x, var, dynamic, quo) {
  
  var <- as_string(var)
  # predictors <- all.vars(x)
  model_quo <- quo_set_expr(quo = quo, expr = x)
  class(model_quo) <- c("daps_nonfitter_quo", class(model_quo))
  
  tibble(
    var = var,
    dynamic = dynamic,
    # predictors = list(predictors),
    model_quo = list(model_quo)
  )
}