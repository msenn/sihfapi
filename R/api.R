#' Generi Call to SIHF API
#'
#' @param path Character string for path to desired API endpoint
#' @param query Search query; see examples and \code{?httr::modify_url}
#'
#' @return httr response object
#' @import httr
#'
#' @examples
#' # Query game list
#' /dontrun{
#' resp <- sihf_api("/statistic/api/cms/table", query = list(
#'   alias       = "results",
#'   searchQuery = "1,8,10,11//1,8,9,20,47,48,50,90,81",
#'   filterQuery = "2017/1/01.09.2016-12.09.2017",
#'   filterBy    = "Season,League,Date"
#'   ))
#' }
sihf_api <- function(path, query) {
  url <- modify_url("https://dvdata.sihf.ch", path = path)
  GET(url, query = query)
}


#' Get Results Table
#'
#' Retrieve a summary table from SIHF API. This functionality is particularly
#' useful to compile a list of game IDs that can be user in \code{get_gameDetail}.
#'
#' @param filter Named vector of fields and values to filter query on. SIHF
#'   requires filter. Details on supported filtering options are unknown. Known
#'   options include \itemize{ \item{Season (e.g. "2017"); this filter appears
#'   to be required} \item{Date (e.g. "06.04.2017" or "01.09.2016-05.03.2018")}
#'   \item{League (e.g. "1" for National League )} \item{Phase (e.g. "2443", key
#'   not fully understood)} }
#'
#' @return httr response object
#' @export
#'
#' @examples
#' results <- get_results(c(Season = "2017", Date = "09.10.2016"))
#' content(results, as = "text")
get_results <- function(filter = c(Season = 2017)) {
  query <- list(
    alias       = "results",
    searchQuery = "1,8,10,11//1,8,9,20,47,48,50,90,81",
    filterQuery = paste(filter, collapse = "/"),
    filterBy    = paste(names(filter), collapse = ",")
  )

  sihf_api("/statistic/api/cms/table", query = query)
}


#' Get Game Detail
#'
#' @param gameId A valid SIHF game ID. Use \code{get_results()} to rerieve
#'   filtered lists of game summaries that include game IDs.
#'
#' @return httr response object
#' @export
#'
#' @examples
#' gameDetail <- get_gameDetail(20171105078373)
#' content(gameDetail, as = "text")
get_gameDetail <- function(gameId) {
  query = list(
    alias        = "gameDetail",
    searchQuery = gameId
  )

  sihf_api("/statistic/api/cms/gameoverview", query)
}
