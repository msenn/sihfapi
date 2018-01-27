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



#' Parse Percent
#'
#' Returns a numeric fraction instead from character percentage.
#'
#' @param x A character vector of percent numbers
#' @param ... Further parameters passed to \code{parse_number}
#'
#' @return Numeric vector
parse_percent <- function(x, ...) {
  parse_number(x, ...) / 100
}


#' Parse Minutes and Seconds
#'
#' @param x Time in 'm:s'
#'
#' @return Time in seconds
parse_ms <- function(x) {
  as.integer(as.numeric(lubridate::ms(x)))
}


#' Parse Hours and Minutes
#'
#' @param x Time in 'h:m'
#'
#' @return Time in seconds
parse_hm <- function(x) {
  as.integer(as.numeric(lubridate::hm(x)))
}


#' Get Field Alias from Header
#'
#' Internal function to get alias vector from header element
#'
#' @param header Header element
#'
#' @return Vector of field aliases
get_alias_from_header <- function(header) {
  header %>%
    map("alias") %>%
    unlist()
}
