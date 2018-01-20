#' Parse Summary
#'
#' @inheritParams parse.gameDetail
#'
#' @return Parsed summary information from gameDetail
#'
#' @export
#'
#' @examples
#' gameDetail <- fetch_gameDetail(20171105078373)
#' parse.summary(gameDetail)
parse.summary <- function(gameDetail) {
  summary <- gameDetail$content$summary

  parse.summary.periods(summary$periods)
}

# Periods ---------------------------------------------------------------------

#' Parse Summary Periods
#'
#' @param periods List of summary period elements
#'
#' @return List of tibbles 'fouls' and 'goals' across all periods
parse.summary.periods <- function(periods) {
  res <- periods %>%
    map(parse.summary.period) %>%
    map(enframe) %>%
    set_names(as.character(1:3)) %>%
    enframe("period") %>%
    unnest() %>%
    spread(name, value)

  list(
    fouls = bind_rows(res$fouls),
    goals = bind_rows(res$goals)
  )
}

#' Parse Summary Period
#'
#' @param period Single summary period element
#'
#' @return List of tibbles containing information in summary period elements
parse.summary.period <- function(period) {
  list(
    fouls = parse.summary.period.child(period$fouls),
    goals = parse.summary.period.child(period$goals)
  )
}

#' Parse Summary Period Element
#'
#' @param x Summary period element ('fouls' or 'goals')
#'
#' @return Parsed tibble of information contained by summary period element
parse.summary.period.child <- function(x) {
  x %>%
    bind_rows() %>%
    mutate_at(vars(matches("[tT]ime")), parse_ms)
}

