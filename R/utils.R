# Utilities functions

#' First To Upper
#'
#' Convert first character of string to upper case (leaving all others as is).
#'
#' @param x A string vector
#'
#' @return String vector with the first character of every element converte to upper case.
#' @export
#'
#' @examples
#' x <- "some string"
#' ut_firstToUpper(x)
ut_firstToUpper <- function(x) {
  first <- toupper(substr(x, 1, 1))
  rest <- substr(x, 2, nchar(x))
  paste0(first, rest)
}
