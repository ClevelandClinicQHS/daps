#' Create a state transition framework
#'
#' Adds a state transition framework to a `daps` object.
#'
#' @param daps A [`daps`]-class object to add a state-transition framework to.
#'
#' @param state A character vector of state names. Must be the same nonzero
#'   lenth as `nodes` and `transitions`.
#'
#' @param nodes A list the same nonzero length as `state` and `transitions`. It
#'   must contain character vectors, each naming the nodes (or variables) that
#'   should be predicted under the corresponding `state`.
#'
#' @param transitions A list the same nonzero length as `state` and `nodes`. It
#'   must contain one-sided formulas, each containing an expression that
#'   produces one of the elements of `state` when evaluated in the context of a
#'   row of temporal data (provided to [simulate()]).
#'
#' @return The input `daps` with the added component `$metastate_model`, an
#'   object of class `daps_metastate_model`.
#'
#' @importFrom dplyr case_when
#' @export
add_metastate_model <- function(daps, state, nodes, transitions) {
  
  if (!is_valid_char_vec(state)) {
    stop(
      "state must be a character vector of nonzero length\n",
      "and having no missingness nor duplicates"
    )
  }
  
  if (!all(vapply(nodes, is_valid_char_vec, logical(1L)))) {
    stop(
      "nodes must be a list of character vectors,\n",
      "each of nonzero length and having no missingness nor duplicates."
    )
  }
  
  if (!all(vapply(transitions, is_formula, logical(1L), lhs = FALSE))) {
    stop("transitions must be a list of formulas, none having a lefthand side")
  }
  
  if ((length_state <- length(state)) != length(nodes) ||
      length_state != length(transitions) ||
      length_state == 0L) {
    stop(
      "Length of states, nodes, and transitions\n",
      "must all be vectors of equal, non-zero length"
    )
  }
  
  transitions <- lapply(transitions, as_quosure)
  
  caller_env <- caller_env()
  
  daps$metastate_model <-
    dplyr::tibble(
      state = state,
      nodes = nodes,
      transitions = transitions
    ) %>% 
    structure(class = c("daps_metastate_model", class(.)))
  
  daps
}



# metastate_model <- function(states, nodes, transitions) {
#   
#   transitions <- transitions %>% lapply(f_rhs) %>% lapply(call_standardise)
#   
#   dplyr::tibble(state = states, nodes = nodes, transitions = transitions) %>%
#     new_metastate_model() 
# }
# 
# new_metastate_model <- function(x, ..., class = character()) {
#   structure(x, ..., class = c(class, "daps_metastate_model", class(x))) 
# }
# 
# 
# as_metastate_model <- function(x, ...) {
#   UseMethod("as_metastate_model")
# }
# 
# 
# as_metastate_model.default <- function(x, ...) {
#   new_metastate_model(x, ...)
# }
