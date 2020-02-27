
#' @export
#' @importFrom dplyr lag
dplyr::lag


#' @export
slide_median <- function(.x, n = Inf, na.rm = FALSE, .fill = NA)
  slide_fn(.x, median, n, na.rm = na.rm, .fill = .fill)

#' @export
slide_mean <- function(.x, n = Inf, na.rm = FALSE, .fill = NA) 
  slide_fn(.x, mean, n, na.rm = na.rm, .fill = .fill)

#' @export
slide_min <- function(.x, n = Inf, na.rm = FALSE, .fill = NA) 
  slide_fn(.x, min, n, na.rm = na.rm, .fill = .fill)

#' @export
slide_max <- function(.x, n = Inf, na.rm = FALSE, .fill = NA) 
  slide_fn(.x, max, n, na.rm = na.rm, .fill = .fill)

#' @import rlang
#' @export
slide_fn <- function(.x, .f, n = Inf, ..., .fill = NA) {
  
  if (!is_scalar_atomic(.fill)) {
    stop("\n.fill must be a single atomic value")
  }
  
  if (!(is_scalar_integerish(n) && n > 0L) &&
      !all.equal(Inf, n, check.attributes = FALSE)) {
    stop("\nn must be a positive integer or Inf")
  }
  
  out <- c(.fill, slider::slide_vec(.x[-length(.x)], .f, .before = n - 1L))
  
  out[is.na(out)] <- .fill
  
  out
}





rowwise_fns <- list(
  
  lag =
    function(x, n = 1L, default = NA, ...)
      if (
        (n <- .__daps_sim_row__. - n) < 1L ||
        is.na(
          x <-
          eval(substitute(x), list(.__daps_sim_row__. = n), parent.frame())
        )
      ) default else x,
  
  slide_fn = 
    function(.x, ...) 
      .__daps_slide_expr__.(
        x = substitute(.x),
        ...,
        .__daps_eval_env__. = parent.frame()
      ),
  slide_median =
    function(.x, ...)
      .__daps_slide_expr__.(
        x = substitute(.x),
        .f = median,
        ...,
        .__daps_eval_env__. = parent.frame()
      ),
  slide_mean = 
    function(.x, ...)
      .__daps_slide_expr__.(
        x = substitute(.x),
        .f = mean,
        ...,
        .__daps_eval_env__. = parent.frame()
      ),
  slide_min = 
    function(.x, ...)
      .__daps_slide_expr__.(
        x = substitute(.x),
        .f = min,
        ...,
        .__daps_eval_env__. = parent.frame()
      ),
  slide_max = 
    function(.x, ...)
      .__daps_slide_expr__.(
        x = substitute(.x),
        .f = max,
        ...,
        .__daps_eval_env__. = parent.frame()
      ),
  
  .__daps_slide_expr__. = 
    function(x, .f, n = Inf, ..., .fill = NA, .__daps_eval_env__.)
      if (
        .__daps_sim_row__. == 1L ||
        is.na(
          x <-
          .f(
            eval(
              x,
              list(
                .__daps_sim_row__. = 
                max(.__daps_sim_row__. - n, 1L):(.__daps_sim_row__. - 1L)
              ),
              .__daps_eval_env__.
            ),
            ...
          )
        )
      ) .fill else x,
      
  # .__daps_safe_subset__. = function(x, i) if (any(i < 1L)) NA else x[i],
  .__daps_subset__. = `[`
)
