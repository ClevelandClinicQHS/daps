
#' Simulate a [`daps`] object.
#'
#' Simulates data from `from` to `to` for each subject using the
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
#' @param from,to Each should be a nonzero integer corresponding to the
#'   time point (i.e., the value of `t` as in the `longitudinal` data set) when
#'   simulation should begin and end. These can both be higher than the highest
#'   value(s) of the `t` column in `longitudinal`. `to` must not be smaller
#'   than `from`.
#' @param start_metastate A character string indicated the initial state of the
#'   subjects (i.e., at `t = from - 1`). Must be one of the states specified
#'   in [add_metastate_model()].
#'
#' @return A list of [`tibble`][tibble::tibble]`s` with length `s` with
#'   simulated data.
#'
#' @import rlang
#' @importFrom purrr pmap_dfr
#' @export
simulate <- function(daps, 
                     static = NULL, 
                     longitudinal = NULL, 
                     from,
                     to,
                     h = NULL,
                     forward = NULL,
                     impute = FALSE,
                     lookback_steps = 5L,
                     s = 1L,
                     seed = NULL) {
  
  models <- daps$pred_setup$models
  mm <- daps$pred_setup$mm
  sim_row_env <- daps$pred_setup$sim_row_env
  
  longitudinal <-
    validate_data(longitudinal, data = "longitudinal", action = "simulate")
  
  static <-
    validate_data(
      static, 
      data = "static", 
      action = "simulate",
      long_ids = longitudinal$id,
      metastates = mm$metastate
    )
  
  if (!is.null(forward)) {
    h <- forward
    from <- to <- "last"
  }
  
  valid_from_to <- validate_from_to(from, to, static, longitudinal, h)
  from <- valid_from_to$from
  to <- valid_from_to$to
  
  longitudinal <-
    dplyr::nest_join(static, longitudinal, by = "id") %>%
    dplyr::pull()
  
  static <- purrr::transpose(static)
  
  if (!is.null(seed)) set.seed(seed)
  
  if (is.null(h)) {
    
    data <-
      pmap_dfr(
        list(
          static = static,
          longitudinal = longitudinal,
          t_start = from,
          t_end = to,
          keep_t_start = from
        ),
        setup_single_sim,
        impute = impute,
        lookback_steps = lookback_steps,
        models = models,
        mm = mm,
        sim_row_env = sim_row_env
      )
    
    rm(static, longitudinal, from, to, valid_from_to)
    
    out <-
      replicate(
        n = s,
        pmap_dfr(
          data,
          run_simulation,
          impute,
          mm,
          models,
          sim_row_env
        ), 
        simplify = FALSE
      )
    
  } else {
    
    from_to_seqs <- valid_from_to %>% unname() %>% purrr::pmap(`:`)
    
    data <- 
      pmap_dfr(
        list(
          static = static,
          longitudinal = longitudinal,
          from_to_seq = from_to_seqs
        ),
        function(static, longitudinal, from_to_seq) {
          pmap_dfr(
            list(
              t_start = from_to_seq + 1L,
              t_end = from_to_seq + max(h),
              keep_t_start = from_to_seq + min(h)
            ),
            setup_single_sim,
            static = static,
            longitudinal = longitudinal,
            impute = impute,
            lookback_steps = lookback_steps,
            mm = mm
          )
        }
      )
    
    new_t_h <- 
      purrr::map_dfr(
        from_to_seqs,
        function(from_to_seq) tidyr::expand_grid(t = from_to_seq, h = h)
      )
    
    rm(static, longitudinal, from, to, valid_from_to, from_to_seqs)
    
    out <-
      replicate(
        n = s,
        change_t_h(
          pmap_dfr(
            data,
            run_simulation,
            impute,
            mm,
            models,
            sim_row_env
          ), 
          new_t_h = new_t_h
        ),
        simplify = FALSE
      )
  }
  
  if (s > 1L) {
    out <- set_names(out, paste("Simulation", seq_len(s)))
  } else {
    out <- out[[1L]]
  }
  
  out
}


#' @import rlang
#' @importFrom dplyr filter arrange
#' @importFrom tidyr complete fill
setup_single_sim <-  function(static,
                              longitudinal,
                              t_start,
                              t_end,
                              keep_t_start,
                              impute,
                              lookback_steps,
                              models,
                              mm,
                              sim_row_env) {
  
  longitudinal <- filter(longitudinal, .data$t < .env$t_start)
  
  if (identical(impute, "locf")) {
    t_before_sim <- t_start - 1L
    longitudinal <- longitudinal %>% 
      complete(t = min(.env$t_before_sim, .data$t):.env$t_before_sim) %>% 
      arrange(.data$t) %>% 
      fill(-"t") %>% 
      complete(t = .env$t_start:.env$t_end) %>% 
      arrange(.data$t)
  } else {
    longitudinal <- longitudinal %>% 
      complete(t = min(.env$t_start - 1L, .data$t):.env$t_end) %>% 
      arrange(.data$t)
  }
  
  data <- c(static[setdiff(names(static), "starting_metastate")], longitudinal)
  
  row <- match(t_start, longitudinal$t)
  row_end <- match(t_end, longitudinal$t)
  keep_rows <- match(keep_t_start, longitudinal$t):row_end
  
  if (isTRUE(impute)) {
    
    candidate_rows <- head(row:1L, n = min(lookback_steps + 1L, row))
    
    for (row in candidate_rows) {
      sim_row_env[[".__daps_sim_row__."]] <- row
      
      data2 <- data
      
      data2[["metastate"]][[row - 1L]] <- static[["starting_metastate"]]
      
      metastate <-
        data2[["metastate"]][row] <-
        eval_tidy(
          mm[["transitions"]][[
            match(static[["starting_metastate"]], mm[["metastate"]])
            ]],
          data
        )
      
      nodes <- mm[["nodes"]][[match(metastate, mm$metastate)]]
      
      for (var in nodes) {
        pred <- predict_with_imputation(var, data2, row, models, sim_row_env)
        if (is.na(pred)) break
        data2 <- attr(pred, "data")
        data2[[var]][row] <- pred
      }
      
      if (!is.na(pred)) break
    }
    
    if (is.na(pred)) {
      stop(
        "\nimputation not possible for variable ", var,
        " for subject with id = ", static$id,
        "\nwhen simulating from ", t_start, " to ", t_end
      )
    }
    
    data <- data2
    sim_rows <- if (row == row_end) integer() else (row + 1L):row_end
  } else {
    data[["metastate"]][[row - 1L]] <- static[["starting_metastate"]]
    sim_rows <- row:row_end
  }
  
  tibble(
    data = list(data),
    sim_rows = list(sim_rows),
    keep_rows = list(keep_rows)
  )
}



#' @importFrom tibble as_tibble
run_simulation <- function(data,
                           sim_rows,
                           keep_rows,
                           impute,
                           mm,
                           models,
                           sim_row_env) {
  
  for (row in sim_rows) {
    sim_row_env[[".__daps_sim_row__."]] <- row
    
    data[["metastate"]][[row]] <-
      eval_tidy(
        mm[[
          match(data[["metastate"]][row - 1L], mm[["metastate"]]),
          "transitions"
          ]],
        data
      )
    
    for (var in
         mm[["nodes"]][[match(data[["metastate"]][[row]], mm[["metastate"]])]])
      for (model in which(models[["var"]] == var))
        if (!anyNA(eval_tidy(models[["check_nonimputable"]][[model]], data)) &&
            !anyNA(eval_tidy(models[["check_imputable"]][[model]], data)) &&
            !is.na(p <- eval(models[["pred_expr"]][[model]], enclos = NULL))) {
          data[[var]][row] <- p
          break
        }
  }
  
  as_tibble(data)[keep_rows, ]
}



#' @import rlang
predict_with_imputation <- function(var, data1, row, models, sim_row_env) {
  
  var_pred <- NA
  
  for (model in which(models[["var"]] == var)) {
    
    if (anyNA(eval_tidy(models[["check_nonimputable"]][[model]], data1))) next
    
    imputable_NAs <-
      which(is.na(eval_tidy(models[["check_imputable"]][[model]], data1)))
    
    data <- data1
    
    if (length(imputable_NAs)) {
      
      dep_rows <- row - models[["imputable_lags"]][[model]]
      
      if (any(dep_rows[imputable_NAs] < 1L)) next
      
      data2 <- data
      
      for (i in imputable_NAs) {
        
        dep_var <- models[["imputable_vars"]][[model]][i]
        dep_row <- dep_rows[i]
        
        dep_var_pred <-
          predict_with_imputation(
            var = dep_var,
            data1 = data2, 
            row = dep_row, 
            models = models, 
            sim_row_env = sim_row_env
          )
        
        if (is.na(dep_var_pred)) break
        
        data2 <- attr(dep_var_pred, "data")
        data2[[dep_var]][[dep_row]] <- dep_var_pred
      }
      
      if (is.na(dep_var_pred)) next
      
      data <- data2
    }
    
    sim_row_env[[".__daps_sim__row__."]] <- row
    var_pred <- eval(models[["pred_expr"]][[model]], enclos = NULL)
    
    if (!is.na(var_pred)) {
      data[[var]][row] <- var_pred
      break
    }
  }
  
  if (!is.na(var_pred)) attr(var_pred, "data") <- data
  
  var_pred
}




change_t_h <- function(x, new_t_h) {
  old_ncol <- length(x)
  t_i <- match("t", names(x))
  x[c("t", "h")] <- new_t_h
  x[c(seq_len(t_i), old_ncol + 1L, (t_i + 1L):old_ncol)]
}
