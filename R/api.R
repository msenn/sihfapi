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
#' \dontrun{
#' resp <- sihf_api("/statistic/api/cms/table", query = list(
#'   alias       = "results",
#'   searchQuery = "1,8,10,11//1,8,9,20,47,48,50,90,81",
#'   filterQuery = "2017/1/01.09.2016-12.09.2017",
#'   filterBy    = "Season,League,Date"
#'   ))
#' }
sihf_api <- function(path, query) {
  url <- modify_url("https://dvdata.sihf.ch", path = path)

  resp <- GET(url, query = query)
  if(http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(
    content(resp, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  structure(
    list (
      content = parsed,
      path = path,
      query = query,
      response = resp
    ),
    class = "sihf_api"
  )
}


#' Print Object of Class 'sihf_api'
#'
#' @param x Return object of \code{sifh_api()}
#' @inheritParams base::print
#'
#' @export
#'
#' @return x (invisibly)
print.sihf_api <- function(x, ...) {
  cat("<SIHF ", x$query$alias, ">\n\n", sep = "")

  if (x$content$alias == "gameDetail") {
    with(
      x$content,
      cat(
        # Season / league
        paste(
          season,
          league$name
        ),

        # Phase / date
        paste(
          phase$name,
          sprintf("(%s)", as.Date(as.POSIXct(startDateTime)))
        ),

        # Score
        paste(
          details$homeTeam$acronym,
          result$homeTeam,
          ":",
          details$awayTeam$acronym,
          result$awayTeam
        ),
        sep = "\n"
      )
    )

    } else if (x$content$alias == "results") {
      cat(
        paste("Number of games:", length(x$content$data)),
        "Filter:",
        paste(
          sprintf(
            "  %s = %s",
            strsplit(x$query$filterBy, ",")[[1]],
            strsplit(x$query$filterQuery, "/")[[1]]
          ),
          collapse = "\n"
        ),
        sep = "\n"
      )
    }

  invisible(x)
}

#' Fetch Results Table
#'
#' Retrieve a summary table from SIHF API. This functionality is particularly
#' useful to compile a list of game IDs that can be user in
#' \code{get_gameDetail}.
#'
#' The date filter of the SIHF API functions in a non-strict way: if no games
#' matching all other filters took place on the specified date (or in the
#' interval), the API will return the last games before that date / interval
#' matching the filters. That is, if we query the API for a date where no games
#' took place, it will still return games.
#'
#' @param filter Named vector of fields and values to filter query on. SIHF
#'   requires filter. Details on supported filtering options are unknown. Known
#'   options include \itemize{ \item{Season (e.g. "2017"); this filter appears
#'   to be required} \item{Date (e.g. "06.04.2017" or "01.09.2016-05.03.2018")}
#'   \item{League (e.g. "1" for National League )} \item{Phase (e.g. "2443", key
#'   not fully understood)} }.
#'
#'   As filter is required, passing \code{filter = NULL} (default) returns
#'   current season (new season starts being returned from July).
#'
#' @return httr response object
#' @export
#'
#' @examples
#' results <- fetch_results(c(Season = "2017", Date = "09.10.2016"))
fetch_results <- function(filter = NULL) {

  if (is.null(filter) || !"Season" %in% names(filter)) {
    today <- Sys.Date()
    currentYear <- as.numeric(format(today, "%Y"))
    currentSeason <- currentYear + (format(today, "%m") > "06")
    filter <- c(Season = currentSeason, filter)
  }

  query <- list(
    alias       = "results",
    searchQuery = "1,8,10,11//1,8,9,20,47,48,50,90,81",
    filterQuery = paste(filter, collapse = "/"),
    filterBy    = paste(names(filter), collapse = ",")
  )

  sihf_api("/statistic/api/cms/table", query = query)
}


#' Fetch Game Detail
#'
#' @param gameId A valid SIHF game ID. Use \code{get_results()} to rerieve
#'   filtered lists of game summaries that include game IDs.
#'
#' @return httr response object
#' @export
#'
#' @examples
#' gameDetail <- fetch_gameDetail(20171105078373)
fetch_gameDetail <- function(gameId) {
  query = list(
    alias        = "gameDetail",
    searchQuery = gameId
  )

  Sys.sleep(mean(runif(25) * 3))
  sihf_api("/statistic/api/cms/gameoverview", query)

}
