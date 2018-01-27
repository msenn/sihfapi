#' Parse Line-Up
#'
#' @inheritParams parse.gameDetail
#'
#' @return List with three elements: "gameId", "players" and "coach"
#'   with line-ups of both home and away team.
#' @export
#'
#' @examples
#' gameDetail <- fetch_gameDetail(20171105078373)
#' parse.gameDetail.lineUp(gameDetail)
parse.gameDetail.lineUp <- function(gameDetail) {
  nested <- gameDetail %$%
    content %$%
    lineUps %>%
    enframe("team", "lineUp") %>%
    mutate(
      lineUp = lineUp %>%
        map(parse.gameDetail.lineUp.team),
      players = lineUp %>%
        map("players"),
      coach = lineUp %>%
        map("coach")
    ) %>%
    select(-lineUp)

  players <- nested %>%
    select(team, players) %>%
    unnest()

  coaches <- nested %>%
    select(team, coach) %>%
    unnest()

  list(
    players = players,
    coaches = coaches
  )
}


#' Parse Team Line-Up
#'
#' @param lineUp.team Team Line-Up
#'
#' @return A list with two elements "players" and "coach"
parse.gameDetail.lineUp.team <- function(lineUp.team) {
  df <- lineUp.team %>%
    enframe("position", "lineUp")

  ## Parse players lineUp
  goalkeepers <- df %>%
    filter(position == "goalkeepers") %>%
    mutate(lineUp = map(lineUp, parse.gameDetail.lineUp.goalkeepers)) %>%
    unnest()

  field <- df %>%
    filter(position %in% c("defenders", "forwarders")) %>%
    mutate(lineUp = map(lineUp, parse.gameDetail.lineUp.field)) %>%
    unnest()

  captain <- df %>%
    filter(position == "captain") %$%
    lineUp %>%
    unlist

  topscorer <- df %>%
    filter(position == "topscorer") %$%
    lineUp %>%
    unlist

  players <- # Combine
    bind_rows(goalkeepers, field) %>%
    mutate(
      captain = (playerId == captain),
      topscorer = (playerId == topscorer)
    )

  ## Parse coach lineUp
  coach <- df %>%
    filter(position == "coach") %$%
    lineUp %>%
    flatten() %>%
    map(~ if(is.null(.x)) NA else .x) %>%
    bind_cols()

  ## Return
  list(players = players, coach = coach)
}


#' Parse Goalkeepers Entry in Team Line-Up
#'
#' @param lineUp.goalkeepers Goalkeepers Entry in Team Line-Up
#'
#' @return Data frame with line and playerId
parse.gameDetail.lineUp.goalkeepers <- function(lineUp.goalkeepers) {
  lineUp.goalkeepers %>%
    enframe("line", "playerId") %>%
    mutate(playerId = unlist(playerId))
}


#' Parse Field Player Entry in Team Line-Up
#'
#' @param lineUp.field Field Player Entry in Team Line-Up
#'
#' @return Data frame with side, line and playerId
parse.gameDetail.lineUp.field <- function(lineUp.field) {
  lineUp.field %>%
    enframe("side") %>%
    mutate(
      value = value %>%
        map(enframe, name = "line", value = "playerId")
    ) %>%
    unnest() %>%
    mutate(playerId = unlist(playerId))
}



