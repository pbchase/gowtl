#' get_player_summary
#'
#' @param team_detail_file - the full path to an html page with player match
#'   level details for a single team
#'
#' @return - a data frame of player summary data for the team described in `team_detail_file`
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' team_detail_file <- here::here(
#'   "input",
#'   "GOWTL score details",
#'   "3.0 Division - 300 Club Everlasting Lobstoppers.html"
#'   )
#' get_player_summary(team_detail_file)
#' }
get_player_summary <- function(team_detail_file) {
  team_detail_list <-
    rvest::read_html(team_detail_file) |>
    rvest::html_elements(xpath = "//div[@class='body container_24']") |>
    rvest::html_elements(xpath = "//table") |>
    rvest::html_table()

  team_detail <- team_detail_list[[2]] |>
    janitor::clean_names()

  divison_and_team <-
    team_detail |>
    dplyr::slice_head(n = 1) |>
    dplyr::select("x1") |>
    dplyr::mutate(division = stringr::str_extract(.data$x1, "[0-9.\\- ]+ Division")) |>
    dplyr::mutate(team = stringr::str_replace(.data$x1, "[0-9.\\- ]+ Division -\\s+", "")) |>
    dplyr::select("division", "team")

  team_detail_col_names <- c(
    "name",
    "win_and_line_1",
    "win_and_line_2",
    "win_and_line_3",
    "win_and_line_4",
    "win_and_line_5",
    "win_and_line_6",
    "win_and_line_7",
    "win_and_line_8",
    "win",
    "loss",
    "total",
    "win_percent"
  )

  colnames(team_detail) <- team_detail_col_names

  team_win_loss_and_line <-
    team_detail |>
    # ignore header facts we will re-add later
    dplyr::filter(!stringr::str_detect(.data$name, "Division")) |>
    dplyr::filter(!stringr::str_detect(.data$name, "Team Member")) |>
    # move "Sub" to its own column
    dplyr::mutate(sub = stringr::str_detect(.data$name, "\\(Sub\\)")) |>
    dplyr::mutate(name = stringr::str_replace(.data$name, " \\(Sub\\)", "")) |>
    dplyr::group_by(.data$name) |>
    # We probably won't use this, but preserve the row number in a way
    # that allows us to easily make a unique match number
    dplyr::mutate(person_row = dplyr::row_number() * 100) |>
    dplyr::ungroup() |>
    dplyr::select("name", "sub", "person_row", dplyr::starts_with("win_and_line")) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("win_and_line"),
      names_prefix = "win_and_line_",
      names_to = "match_column",
      values_to = "win_and_line"
    ) |>
    # we probably won't use this, but make a unique match number just in case
    dplyr::mutate(match_num = as.numeric(.data$match_column) + .data$person_row) |>
    dplyr::select(-c("person_row", "match_column")) |>
    # split win/loss and line into their own columns
    dplyr::mutate(
      win = dplyr::case_when(
        stringr::str_detect(.data$win_and_line, "W") ~ 1,
        stringr::str_detect(.data$win_and_line, "L") ~ 0,
        T ~ as.numeric(NA)
      ),
      line = as.numeric(stringr::str_extract(.data$win_and_line, "[0-9]"))
    ) |>
    dplyr::select(-"win_and_line") |>
    # re-add divisons and team
    dplyr::bind_cols(divison_and_team)

  player_summary <-
    team_win_loss_and_line |>
    # Group by all of the facts we want to preserve even though
    # name is the only thing that varies
    dplyr::group_by(.data$division, .data$team, .data$name, .data$sub) |>
    # Create all of the precious summary facts
    dplyr::summarise(
      wins = sum(.data$win, na.rm = T),
      matches = sum(!is.na(.data$win), na.rm = T),
      losses = .data$matches - .data$wins,
      win_ratio = .data$wins / .data$matches,
      line_avg = mean(.data$line, na.rm = T),
      move_up = dplyr::case_when(
        .data$matches >= 4 & .data$line_avg <= 2.4 & .data$win_ratio >= 0.7 ~ 1,
        TRUE ~ 0
      ),
      move_down = dplyr::case_when(
        .data$matches >= 4 & .data$line_avg >= 4.0 & .data$win_ratio <= 0.2 ~ 1,
        TRUE ~ 0
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(.data$move_up), .data$move_down, dplyr::desc(.data$win_ratio))

  return(player_summary)
}
