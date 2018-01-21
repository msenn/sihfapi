#' Parse Result
#'
#' @inheritParams parse.gameDetail
#'
#' @return Tibble containing parsed information of \code{gameDetail$content$result}
parse.result <- function(gameDetail) {
  scores <- result$scores %>%
    bind_rows() %>%
    set_names(paste("score", names(.), sep = "_"))

  sogs <- result$sogs %>%
    bind_rows() %>%
    set_names(paste("sog", names(.), sep = "_"))

  left_join(scores, sogs, by = c(score_name = "sog_name", score_indicator = "sog_indicator")) %>%
    rename(name = score_name, indicator = score_indicator) %>%
    add_row(name = "total") %>%
    mutate_at(
      vars(starts_with("score_"), starts_with("sog_"), indicator),
      as.integer
    ) %>%
    mutate(
      score_homeTeam = if_else(name == "total", as.integer(result$homeTeam), score_homeTeam),
      score_awayTeam = if_else(name == "total", as.integer(result$awayTeam), score_awayTeam),
      sog_homeTeam   = if_else(name == "total", sum(sog_homeTeam, na.rm = TRUE), sog_homeTeam),
      sog_awayTeam   = if_else(name == "total", sum(sog_awayTeam, na.rm = TRUE), sog_awayTeam)
    )
}


