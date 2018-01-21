#' Parse Details
#'
#' @inheritParams parse.gameDetail
#'
#' @return Parsed content of \code{gameDetail$content$details}
parse.details <- function(gameDetail) {
  details <- gameDetail$content$details

  venue <- details$venue %>%
    bind_rows() %>%
    mutate_at(c("spectators", "zip"), as.integer)



  referees <- bind_rows(
    bind_rows(details$referees) %>%
      mutate(role = "referee"),
    bind_rows(details$linesmen) %>%
      mutate(role = "linesman")
  ) %>%
    mutate_at(c("id", "jerseyNumber"), as.integer)

  teams <- bind_rows(
    bind_rows(details$homeTeam) %>%
      mutate(team = "homeTeam"),
    bind_rows(details$awayTeam) %>%
      mutate(team = "awayTeam")
  )

  list(venue = venue, referees = referees, teams = teams)
}



