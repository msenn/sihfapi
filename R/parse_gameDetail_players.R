#' Parse Players
#'
#' @inheritParams parse.gameDetail
#'
#' @return Tibble containing player information. In particular, this matches teamId and jersey number to playerId.
#' @export
#'
#' @examples
#' gameDetail <- fetch_gameDetail(20171105078373)
#' parse.gameDetail.players(gameDetail)
parse.gameDetail.players <- function(gameDetail) {
  inner <- function(x) {
    map(x, ~ ifelse(is.null(.x), NA, .x))
  }

  gameDetail$content$players %>%
    map(inner) %>%
    bind_rows()
}
