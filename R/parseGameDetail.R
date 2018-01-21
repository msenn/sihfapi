#' Parse Game Detail
#'
#' Parse game detail object as returned by \code{fetch_gameDetail()} into a set of flat tibbles.
#'
#' @param gameDetail SIHF API gameDetail object (see \code{fetch_gameDetail()})
#'
#' @return Tibbles containing the parsed information from game detail. Moste information is nested in the list columns.
#' @export
#'
#' @examples
#' gameDetail <- fetch_gameDetail(20171105078373)
#' gameDetail.parsed <- parse.gameDetail(gameDetail)
parse.gameDetail <- function(gameDetail) {
  summary <- parse.summary(gameDetail)
  stats <-  parse.stats(gameDetail)
  lineUp <- parse.lineUp(gameDetail)
  details <- parse.details(gameDetail)

  parse.header(gameDetail) %>%
    bind_cols(
      details$venue %>%
        set_names(paste("venue", names(details$venue), sep = "_"))
    ) %>%
    mutate(
      summary_goals    = list(summary$goals),
      summary_fouls    = list(summary$fouls),

      result           = list(parse.result(gameDetail)),

      stats_players    = list(stats$players),
      stats_goalies    = list(stats$goalies),
      stats.teams      = list(stats$teams),

      lineUp_players   = list(lineUp$players),
      lineUp_coaches   = list(lineUp$coaches),

      players          = list(parse.players(gameDetail)),

      details_teams    = list(details$teams),
      details_referees = list(details$referees)
    )
}
