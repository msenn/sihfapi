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
#' parse.gameDetail(gameDetail)
parse.gameDetail <- function(gameDetail) {
  stats <-  parse.stats(gameDetail)
  lineUp <- parse.lineUp(gameDetail)

  list(
    header = parse.header(gameDetail),

    stats.players = stats$players,
    stats.goalies = stats$goalies,
    stats.teams = stats$teams,

    lineUp.players = lineUp$players,
    lineUp.coaches = lineUp$coaches,

    players = parse.players(gameDetail)
  )
}
