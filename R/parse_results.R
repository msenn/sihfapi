#' Parse Results
#'
#' @param results "Results" output of SIHF Stats API as retrieved by \code{fetch_results()}
#'
#' @return A tibble with the parsed content of results output
#' @export
#'
#' @examples
#' results_raw <- fetch_results(c(Season = 2017))
#' print(results_raw)
#'
#' results <- parse.results(results_raw)
parse.results <- function(results) {
  alias <- results$content$header %>%
    map_chr("alias")

  results$content$data %>%
    map(set_names, alias) %>%
    map(parse.results.item) %>%
    bind_rows() %>%

    mutate_at("date", as.Date, format = "%d.%m.%Y") %>%
    mutate_at("time", parse_hm) %>%
    mutate_at(vars(matches("score_(home|away)Team")), as.integer) %>%

    mutate(gameId = details_gameId) %>%
    select(gameId, everything())
}


#' Results Single Item
#'
#' @param x Single item of \code{results$content$data}
#'
#' @return Tibble with parsed content of x
parse.results.item <- function(x) {
  flat <- c(
    "day",
    "date",
    "time",
    "decision",
    "status"
  ) %>%
    map(~ x[[.x]] %>%
          set_names(.x)
    ) %>%
    flatten() %>%
    bind_cols()

  one_level <- c(
    "homeTeam",
    "awayTeam",
    "score",
    "details",
    "video"
  ) %>%
    map(~ x[[.x]] %>%
          bind_rows() %>%
          set_names(paste(.x, names(.), sep = "_"))
    ) %>%
    bind_cols()

  two_levels <- c("homeTeam", "awayTeam") %>%
    map(
      ~ x[["scorePerPeriod"]][[.x]] %>%
      { # Happens to be empty at times
        if (length(.)) parse.results.item.scorePerPeriod.team(.) %>%
          set_names(paste(.x, names(.), sep = "_")) else tibble()
      }
    ) %>%

    bind_cols() %>%
    { # Happens to be empty at times
      if (dim(.)[2]) set_names(., paste("score", names(.), sep = "_")) else .
    }

  bind_cols(flat, one_level, two_levels)
}

#' Pares Results Single Item ScorePerPeriod Team
#'
#' @param x \code{results$content$data$scorePerPeriod$homeTeam} or \code{results$content$data$scorePerPeriod$homeTeam}
#'
#' @return Tibble with parsed content of x
parse.results.item.scorePerPeriod.team <- function(x) {
  x %>%
    set_names(paste(
      "period", as.character(seq(length(x))), sep = "_"
    )) %>%
    bind_cols()
}
