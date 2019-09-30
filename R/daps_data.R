
daps_data <- function(static, temporal, initialization) {
  new_daps_data(
    list(static = static, temporal = temporal, initialization = initialization)
  )
}

new_daps_data <- function(x, ..., class = character()) {
  structure(x, ..., class = c(class, "daps_data"))
}