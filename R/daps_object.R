#' Create a new `daps` object.
#'
#' The first step in the dynamically adaptive probabilistic system pipeline.
#'
#' This should be piped into either [add_models()] or [add_metastate_model()].
#'
#' @section Components:
#' `daps`-class objects can contain the following components:
#' * **`daps_metastate_model`** added via [add_metastate_model()]. This contains the
#'   state-transition framework.
#' * **`daps_model_table`** added via [add_models()]. This contains individual
#'   variable models.
#'
#' @return An object of class `daps`.
#'
#' @examples
#' daps()
#'
#' @export
daps <- function() new_daps()



new_daps <- function(x = list()) structure(x, class = "daps")
