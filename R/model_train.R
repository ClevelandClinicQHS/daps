#' @import rlang
#' @importFrom purrr pmap_dfr
#' @importFrom tibble tibble
#' @export
train <- function(daps, static, longitudinal) {
  
  static <- validate_data(static, data = "static", action = "train")
  
  longitudinal <-
    validate_data(longitudinal, data = "longitudinal", action = "train")
  
  data <- dplyr::full_join(static, longitudinal, by = "id")
  
  sub_key <- 
    longitudinal %>% 
    colnames() %>% 
    setdiff("id") %>% 
    setNames(nm = .) %>% 
    syms() %>% 
    lapply(call2, .fn = ".__daps_subset__.", quote(.__daps_sim_row__.))
  
  sim_row_env <- env(env_parent(), .__daps_sim_row__. = NA_integer_)
  
  rowwise_fns <- rowwise_fns %>% purrr::map_if(is_closure, set_env, sim_row_env)
    
  list2env(rowwise_fns, envir = sim_row_env)
  
  sim_row_fn <- new_function(NULL, quote(.__daps_sim_row__.), sim_row_env)
  
  model_depends <- 
    daps$model_table %>% 
    dplyr::select(x = "model_quo", "model_id") %>% 
    purrr::pmap(
      validate_predictors, 
      temporal_vars = setdiff(colnames(longitudinal), "id"),
      static_vars = colnames(static)
    )
  
  node_order <- var_order(daps$model_table, model_depends)
  
  mm <-
    daps$metastate_model %>% 
    dplyr::mutate(
      nodes = lapply(.data$nodes, intersect, x = node_order),
      transitions = 
        lapply(
          .data$transitions,
          function(quo) {
            
            expr <- quo %>% quo_get_expr() %>% substitute_q2(sub_key)
            
            env <-
              quo %>% 
              quo_get_env() %>% 
              append_sim_row_env(rowwise_fns, sim_row_fn)
            
            new_quosure(expr, env)
          }
        )
    )
  
  model_sim_envs <- 
    daps$model_table$model_quo %>%
    lapply(quo_get_env) %>% 
    lapply(append_sim_row_env, rowwise_fns, sim_row_fn)
  
  na_checks <-
    purrr::map2_dfr(model_depends, model_sim_envs, make_na_checks, node_order)
  
  trained_models <- lapply(daps$model_table$model_quo, train_model, data = data)
  
  pred_exprs <-
    purrr::map2(trained_models, model_sim_envs, make_pred_expr, sub_key)
  
  daps$trained_models <- 
    daps$model_table %>% 
    dplyr::transmute(.data$model_id, .data$var, model = trained_models)
  
  daps$pred_setup <-
    list2(
      models = 
        tibble(
          var = daps$model_table$var,
          !!!na_checks,
          pred_expr = pred_exprs
        ),
      mm = mm,
      sim_row_env = sim_row_env
    )
  
  daps
}



append_sim_row_env <- function(env, rowwise_fns, sim_row_fn) {
  sim_row_env <- new_environment(rowwise_fns, parent = env)
  env_bind_active(sim_row_env, .__daps_sim_row__. = sim_row_fn)
  sim_row_env
}



train_model <- function(model_quo, ...) {
  UseMethod("train_model")
}



#' @export
train_model.daps_nonfitter_quo <- function(model_quo, ...) {
  model_quo
}


#' @export
train_model.daps_fitter_quo <- function(model_quo, data, ...) {
  
  model_quo_env <- quo_get_env(model_quo)
  
  fitter_fn <- call_fn(model_quo)
  
  fitter_fn_fmls <- fn_fmls_names(fitter_fn)
  
  if (any(fitter_fn_fmls == "formula")) {
    
    model_standardised <- call_standardise(model_quo)
    
    quo_args <- call_args(model_standardised)
    
    formula <- quo_args[["formula"]] %>% eval(model_quo_env, NULL)
    
    class(formula) <- c("daps_formula", class(formula))
    
    environment(formula) <- environment(formula) %>% 
      env(.__daps_variables_by_id__. = .__daps_variables_by_id__.)
    
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
  
  model
}



#' @export
terms.daps_formula <- function(x, ...) {
  
  terms <- NextMethod()
  
  attr(terms, "variables")[[1L]] <- quote(.__daps_variables_by_id__.)
  
  terms
}


#' @import rlang
#' @importFrom tibble as_tibble
.__daps_variables_by_id__. <- function(...) {
  
  caller_env <- parent.frame()
  
  caller_env %>% 
    as.list(all.names = TRUE) %>% 
    as_tibble() %>% 
    dplyr::group_by(.data$id) %>% 
    dplyr::transmute(...) %>%
    dplyr::ungroup() %>% 
    dplyr::select(-"id") %>% 
    as.list() %>% 
    unname()
}




#' @import rlang
#' @importFrom dplyr filter
var_order <- function(model_table, model_depends) {
  concurrent_dependencies <- 
    model_depends %>%
    dplyr::bind_rows() %>% 
    filter(.data$lag == 0L) %>%
    dplyr::select(model_id_downstream = "model_id", "var")
  
  edge_coordinates <-
    model_table %>% 
    dplyr::select("var", model_id_upstream = "model_id") %>% 
    dplyr::inner_join(concurrent_dependencies, by = "var") %>% 
    dplyr::transmute_at(
      .vars = c("model_id_upstream", "model_id_downstream"),
      .funs = match,
      table = model_table$model_id
    ) %>% 
    as.matrix()
  
  dag_matrix <- nrow(model_table) %>% matrix(data = 0L, nrow = ., ncol = .)
  dag_matrix[edge_coordinates] <- 1L
  
  model_order <- Rfast::topological_sort(dag_matrix)
  
  if (identical(model_order, NA)) {
    stop("\nCyclical intra-timeslice model dependency detected.")
  }
  
  model_table$var[model_order] %>% unique(fromLast = TRUE)
}



#' @importFrom dplyr arrange
make_na_checks <- function(depends, env, node_order) {
  
  imputable <- which(depends$lag > 0L)
  
  ordered_imp_depends <-
    depends[imputable, ] %>%
    arrange(match(.data$var, node_order), .data$lag)
  
  check_nonimputable <-
    call2("list", !!!depends$call[-imputable]) %>%
    new_quosure(env)
  
  check_imputable <-
    call2("list", !!!ordered_imp_depends$call) %>% 
    new_quosure(env)
  
  tibble(
    check_nonimputable = list(check_nonimputable),
    check_imputable = list(check_imputable),
    imputable_vars = list(ordered_imp_depends$var),
    imputable_lags = list(ordered_imp_depends$lag)
  )
}
