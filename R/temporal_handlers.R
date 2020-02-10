
rowwise_fns <- list(
  
  lag = 
    
    function(x, n = 1, default = NA, ...) {
      
      if (!is_scalar_integerish(n) || n < 0) {
        stop("n must be a nonnegative, scalar integer")
      }
      
      caller_env <- parent.frame()
      
      new_row <- eval(quote(.__daps_sim_row__.), caller_env) - n
      
      if (new_row < 1) {
        return(default)
      }
      
      eval(substitute(x), list(.__daps_sim_row__. = new_row), caller_env)
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