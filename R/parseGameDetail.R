#' Parse Game Detail
#'
#' Parse game detail object as returned by \code{fetch_gameDetail()} into a set of flat tibbles.
#'
#' @param gameDetail SIHF API gameDetail object (see \code{fetch_gameDetail()})
#'
#' @return List of tibbles containing all data from game detail.
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

  list(
    header = bind_cols(
      parse.header(gameDetail),
      set_names(details$venue, paste("venue", names(details$venue), sep = "_"))
    ),

    summary_goals = summary$goals,
    summary_fouls = summary$fouls,

    result = parse.result(gameDetail),

    stats_players = stats$players,
    stats_goalies = stats$goalies,
    stats.teams = stats$teams,

    lineUp_players = lineUp$players,
    lineUp_coaches = lineUp$coaches,

    players = parse.players(gameDetail),

    details_teams = details$teams,
    details_referees = details$referees
  )
}
