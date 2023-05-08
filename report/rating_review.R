library(tidyverse)
library(rvest)
library(gowtl)

# team_detail_file <- here::here("input", "GOWTL score details", "3.0 Division - 300 Club Everlasting Lobstoppers.html")
# get_player_summary(team_detail_file)

input_folder <- here::here("input", "GOWTL score details")

team_detail_files <- fs::dir_ls(input_folder) %>%
  fs::file_info() %>%
  filter(type == "file") %>%
  filter(str_ends(path, ".html")) %>%
  pull(path)

player_summaries <- purrr::map_dfr(team_detail_files, get_player_summary)

player_summaries %>%
  filter(!sub) %>%
  filter(matches > 0) %>%
  select(-sub) %>%
  mutate(move_up = if_else(move_up == 1, "Review zone", "")) %>%
  mutate(move_down = if_else(move_down == 1, "Review zone", "")) %>%
  write_csv(here::here("output", "GOWTL 3.0 Division rating review.csv"))

player_summaries %>% view()

player_summaries %>%  filter(matches == 0)

player_summaries %>% count(move_up)
player_summaries %>% count(move_down)
player_summaries %>% count(team)
