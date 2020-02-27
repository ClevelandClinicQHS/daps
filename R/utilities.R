
if (getRversion() >= "2.15.1") utils::globalVariables(".")
# if (getRversion() >= "2.15.1") utils::globalVariables(c(".", ":="))

#' @export
#' @importFrom magrittr %>%
magrittr::`%>%`

substitute_q2 <- function(x, env) {
  x <- x
  env <- env
  eval(substitute(substitute(x, env)), NULL, NULL)
}




#' @export
categorical <- function(x, ...) {
  
  length_x <- length(x)
  
  if (!is.character(x) && !is.factor || !length_x || anyDuplicated(x)) {
    stop("\nx must be a factor or character vector with no duplicates")
  }
  
  prob <- c(...)
  
  if (!is.null(prob)) {
    
    if (!is.numeric(prob) || anyNA(prob) || !all(prob >= 0)) {
      stop(
        "\nThe probabilities in ... must be nonnegative numbers between 0 and 1"
      )
    }
    
    if (!is_dictionaryish(prob)) {
      stop("\nEach probability value in ... must have unique name")
    }
    
    names_prob <- names(prob)
    
    if (length(setdiff(names_prob, x))) {
      stop(
        "\nThe following probabilities do not have names present in x:\n",
        paste(names_prob, "=", prob, collapse = ", ")
      )
    }
    
    length_prob <- length(prob)
    
    if (length_x != length_prob) {
      if (length_x == length_prob + 1L) {
        sum_prob <- sum(prob)
        if (sum_prob > 1) {
          stop(
            "\nWhen leaving out one probability value, the sum of the other ",
            "\nprobabilities must not be greater than 1"
          )
        }
        prob <- c(prob, set_names(1 - sum_prob, setdiff(x, names_prob)))
      } else {
        stop(
          "\nEach element of x must have an associated probability, with the ",
          "\noption to leave one element's probability out (its probability ",
          "\nwill be assumed to be 1 - sum(probabilities)"
        )
      }
    }
    
    prob <- prob[x]
  }
  
  sample(as.factor(x), size = 1L, prob = prob)
}
