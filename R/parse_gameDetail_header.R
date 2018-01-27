#' Parse Header
#'
#' @inheritParams parse.gameDetail
#'
#' @return Tibble
#' @export
#'
#' @examples
#' gameDetail <- fetch_gameDetail(20171105078373)
#' parse.gameDetail.header(gameDetail)
parse.gameDetail.header <- function(gameDetail) {
  content <- gameDetail$content

  scalar <- c(
    "round",
    "gameId",
    "season",
    "updateDateTime",
    "startDateTime",
    "endDateTime",
    "videoId",
    "info",
    "alias"
  ) %>%
    map(~ set_names(content[[.x]], .x)) %>%
    flatten() %>%
    bind_cols()

  nested <- c(
    "league",
    "region",
    "group",
    "tournament",
    "qualification",
    "phase",
    "status"
  ) %>%
    map(
      ~ content[[.x]] %>%
        bind_cols() %>%
        set_names(paste(.x, names(.), sep = "_"))
    ) %>%
    bind_cols()

  bind_cols(scalar, nested) %>%

    mutate_at(vars(ends_with("DateTime")), as.POSIXct)
}





