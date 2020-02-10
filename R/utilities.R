
if (getRversion() >= "2.15.1") utils::globalVariables(".")
# if (getRversion() >= "2.15.1") utils::globalVariables(c(".", ":="))


#' @export
#' @importFrom magrittr %>%
magrittr::`%>%`

#' @export
#' @importFrom dplyr lag
dplyr::lag


#' @export
#' @importFrom dplyr case_when
dplyr::case_when
