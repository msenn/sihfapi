# Parse stats

#' Parse Stats Element
#'
#' @param stats.element
#'
#' @return
#' @export
#'
#' @examples
parse.stats.data <- function(stats.data, columnNames) {
  stats.data %>%
    map(
      ~set_names(., columnNames) %>%
        as.tibble()
    ) %>%
    bind_rows()
}
