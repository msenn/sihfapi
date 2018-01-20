#' Parse Stats
#'
#' @inheritParams parse.lineUp
#'
#' @return List of three tibbles: players, goalies, teams
#' @export
#'
#' @examples
#' gameDetail <- fetch_gameDetail(20171105078373)
#' parse.stats(gameDetail)
parse.stats <- function(gameDetail) {
  stats <- gameDetail$content$stats
  list(
    players = list(stats[[1]], stats[[3]]) %>%
      map(parse.stats.players) %>%
      bind_rows(),

    goalies = list(stats[[2]], stats[[4]]) %>%
      map(parse.stats.goalies) %>%
      bind_rows(),

    teams = stats[[5]] %>%
      parse.stats.teams()
  )
}



#' Parse Stats Players
#'
#' @param players \code{gameDetail$content$stats[[1]]} and \code{gameDetail$content$stats[[3]]}
#'
#' @return Parsed goalie stats (one team / game)
#'
#' @examples
#' gameDetail <- fetch_gameDetail(20171105078373)
#' parse.stats.players(gameDetail$content$stats[[1]])
parse.stats.players <- function(players) {
  alias <- players$header %>%
    get_alias_from_header()

  players$data %>%
    map(set_names, alias) %>%
    bind_rows() %>%

    mutate_all(na_if, "-") %>%
    mutate_at(vars(playerNumber, goals:faceoffsLost), "as.integer") %>%
    mutate_at(vars(starts_with("timeOnIce")), parse_ms) %>%
    mutate_at("faceoffsPercent", "parse_percent", na = "-") %>%

    mutate(team = players$description)
}


#' Parse Stats Goalies
#'
#' @param goalies \code{gameDetail$content$stats[[2]]} and \code{gameDetail$content$stats[[4]]}
#'
#' @return Parsed goalie stats (one team / game)
#'
#' @examples
#' gameDetail <- fetch_gameDetail(20171105078373)
#' parse.stats.goalies(gameDetail$content$stats[[2]])
parse.stats.goalies <- function(goalies) {
  alias <- goalies$header %>%
    get_alias_from_header()

  goalies$data %>%
    map(set_names, alias) %>%
    bind_rows() %>%

    mutate_all(na_if, "-") %>%
    mutate_at(vars(playerNumber, goalsAgainst:shotsAgainst, penaltyInMinutes), "as.integer") %>%
    mutate_at("secondsPlayed", parse_ms) %>%
    mutate_at("savesPercentage", "parse_percent", na = "-") %>%
    mutate_at("goalsAgainstAverage", as.numeric) %>%

    mutate(team = goalies$description)
}


#' Parse Stats Team
#'
#' @param teams \code{gameDetail$content$stats[[5]]}
#'
#' @return Parsed teams stats (one game, both teams)
#'
#' @examples
#' gameDetail <- fetch_gameDetail(20171105078373)
#' parse.stats.teams(gameDetail$content$stats[[5]])
parse.stats.teams <- function(teams) {
  description <- teams$header %>%
    map("description") %>%
    unlist()

  teams$data %>%
    map(set_names, description) %>%
    bind_rows() %>%
    mutate(Statistic = Statistic %>%
             str_replace_all("%", " pct") %>%
             str_replace_all(" ", "_") %>%
             str_to_lower()) %>%
    gather("team", "value", -Statistic) %>%
    spread(Statistic, value)
}
