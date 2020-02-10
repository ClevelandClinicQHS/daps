
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
  sim_row_env <- env(env_parent(), .__daps_sim_row__. = NA_integer_)
  sim_row_fn <-
    new_function(
      args = list(),
      body = quote(.__daps_sim_row__.),
      env = sim_row_env
    )
  
  sub_key <- 
    longitudinal %>% 
    colnames() %>% 
    setdiff("id") %>% 
    set_names() %>% 
    syms() %>% 
    lapply(call2, .fn = "[", quote(.__daps_sim_row__.))
  
  mm <-
    daps$metastate_model %>% 
    dplyr::mutate(
      nodes = lapply(.data$nodes, setdiff, y = colnames(static)),
      transitions = 
        lapply(
          .data$transitions,
          function(quo) {
            expr <- pryr::substitute_q(quo_get_expr(quo), env = sub_key)
            env <- env(quo_get_env(quo), !!!rowwise_fns)
            env_bind_active(env, .__daps_sim_row__. = sim_row_fn)
            new_quosure(expr, env)
          }
        )
    )
  
  fits <-
    daps$trained_fits %>% 
    dplyr::mutate(
      predictors = lapply(.data$predictors, setdiff, y = colnames(static)),
      model = 
        lapply(
          .data$model, 
          make_predict_expr, 
          sub_key = sub_key, 
          sim_row_fn = sim_row_fn
        )
    )
  
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
        fits = fits,
        mm = mm,
        sim_row_env = sim_row_env
      )
    
    rm(static, longitudinal, from, to, valid_from_to)
    
    out <-
      replicate(
        n = s,
        pmap_dfr(data, run_simulation, impute, mm, fits, sim_row_env),
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
          pmap_dfr(data, run_simulation, impute, mm, fits, sim_row_env),
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



setup_single_sim <-  function(static,
                              longitudinal,
                              t_start,
                              t_end,
                              keep_t_start,
                              impute,
                              lookback_steps,
                              fits,
                              mm,
                              sim_row_env) {
  longitudinal <- dplyr::filter(longitudinal, .data$t < .env$t_start)
  
  if (identical(impute, "locf")) {
    t_before_sim <- t_start - 1L
    longitudinal <- longitudinal %>% 
      tidyr::complete(t = min(.env$t_before_sim, .data$t):.env$t_before_sim) %>% 
      dplyr::arrange(.data$t) %>% 
      tidyr::fill(-"t") %>% 
      tidyr::complete(t = .env$t_start:.env$t_end) %>% 
      dplyr::arrange(.data$t)
  } else {
    longitudinal <- longitudinal %>% 
      tidyr::complete(t = min(.env$t_start - 1L, .data$t):.env$t_end) %>% 
      dplyr::arrange(.data$t)
  }
  
  data <- c(static[setdiff(names(static), "starting_metastate")], longitudinal)
  
  row <- match(t_start, longitudinal$t)
  row_end <- match(t_end, longitudinal$t)
  keep_row_start <- match(keep_t_start, longitudinal$t)
  
  if (isTRUE(impute)) {
    
    candidate_rows <- head(row:1L, n = min(lookback_steps + 1L, row))
    last_candidate <- candidate_rows[length(candidate_rows)]
    
    for (row in candidate_rows) {
      sim_row_env$.__daps_sim_row__. <- row
      
      metastate <-
        eval_tidy(
          mm$transitions[[match(static$starting_metastate, mm$metastate)]],
          data
        )
      
      nodes <- mm$nodes[[match(metastate, mm$metastate)]]
      
      data2 <- data
      
      for (var in nodes) {
        pred <- data2[[var]][row] <- get_prediction(fits, var, nodes, data2)
        if (is.na(pred)) {
          if (row == last_candidate) {
            stop(
              "\nimputation not possible for variable ", var,
              " for subject with id = ", static$id,
              "\nwhen simulating from ", t_start, " to ", t_end
            )
          }
          break
        }
      }
      
      if (!is.na(pred)) break
    }
    
  }
  
  data$metastate[row - 1L] <- static$starting_metastate
  
  tibble(
    data = list(data),
    row = row,
    row_end = row_end,
    keep_row_start = keep_row_start
  )
}



#' @importFrom tibble as_tibble
run_simulation <- function(data,
                           row,
                           row_end,
                           keep_row_start,
                           impute,
                           mm,
                           fits,
                           sim_row_env) {
  while (row <= row_end) {
    sim_row_env$.__daps_sim_row__. <- row
    
    data$metastate[row] <-
      eval_tidy(
        mm$transitions[[match(data$metastate[row - 1L], mm$metastate)]],
        data
      )
    
    nodes <- mm$nodes[[match(data$metastate[row], mm$metastate)]]
    
    for (var in nodes) {
      data[[var]][row] <- get_prediction(fits, var, nodes, data)
    }
    
    row <- row + 1L
  }
  
  as_tibble(data)[keep_row_start:row_end, ]
}



change_t_h <- function(x, new_t_h) {
  old_ncol <- ncol(x)
  t_i <- match("t", names(x))
  x[c("t", "h")] <- new_t_h
  x[c(seq_len(t_i), old_ncol + 1L, (t_i + 1L):old_ncol)]
}



get_prediction <- function(fits, var, nodes, data) {
  
  for (row in which(fits$var == var)) {
    if (all(fits$predictors[[row]] %in% nodes)) {
      p <- suppressWarnings(suppressMessages(eval(fits$model[[row]])))
      if (!is.na(p)) {
        return(p)
      }
    }
  }
  
  NA
}
