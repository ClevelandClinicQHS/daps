
rowwise_fns <- list(
  
  lag = 
    
    function(x, n = 1, default = NA, ...) {
      
      if (!is_scalar_integerish(n) || n < 0) {
        stop("n must be a nonnegative, scalar integer")
      }
      
      caller_env <- caller_env()
      
      if (get(".__daps_sim_row__.", envir = caller_env) <= n) {
        return(default)
      }
      
      n <- n
      
      env <- list(.__daps_sim_row__. = substitute(.__daps_sim_row__. - n))
      
      x <- do_call_substitute.default(substitute(x), env)
      
      eval(x, caller_env)
    }
)

# # @export
# slide_mean <- function(x, 
#                        n = 1,
#                        lag = 1,
#                        na.rm = FALSE,
#                        .partial = FALSE,
#                        ...) {
#   slide_fn(
#     x = x,
#     .f = mean,
#     n = n,
#     lag = lag,
#     .partial = .partial,
#     na.rm = na.rm,
#     ...
#   )
# }
# 
# 
# 
# 
# # @export
# slide_median <- function(x, 
#                          n = 1,
#                          lag = 1,
#                          na.rm = FALSE,
#                          .partial = FALSE,
#                          ...) {
#   slide_fn(
#     x = x,
#     .f = median,
#     n = n,
#     lag = lag,
#     .partial = .partial,
#     na.rm = na.rm,
#     ...
#   )
# }
# 
# 
# 
# # @export
# slide_min <- function(x, 
#                       n = 1,
#                       lag = 1,
#                       na.rm = FALSE,
#                       .partial = FALSE,
#                       ...) {
#   slide_fn(
#     x = x,
#     .f = min,
#     n = n,
#     lag = lag,
#     .partial = .partial,
#     na.rm = na.rm,
#     ...
#   )
# }
# 
# 
# 
# 
# # @export
# slide_max <- function(x, 
#                       n = 1,
#                       lag = 1,
#                       na.rm = FALSE,
#                       .partial = FALSE,
#                       ...) {
#   slide_fn(
#     x = x,
#     .f = max,
#     n = n,
#     lag = lag,
#     .partial = .partial,
#     na.rm = na.rm,
#     ...
#   )
# }
# 
# 
# # @export
# slide_fn <- function(x, .f, n, lag, .partial, ...) {
#   tsibble::slide(
#     .x = lag(x, n = lag),
#     .f = .f,
#     .size = n,
#     .partial = .partial,
#     ...
#   ) %>% 
#     as.vector(mode(x))
# }