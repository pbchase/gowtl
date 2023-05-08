library(tidyverse)
library(rvest)

league_membership_file <- here::here("input", "510-1682724319-dirplay.csv")
league_membership <- read.csv(league_membership_file) %>%
  janitor::clean_names()

league_membership %>%
  filter(league == "Women's Doubles") %>%
  count(division)

membership_by_rated_team <-
  league_membership %>%
  filter(league == "Women's Doubles") %>%
  mutate(name = paste0(last, ", ", first)) %>%
  mutate(rating = str_sub(division, 1, 3)) %>%
  group_by(name) %>%
  arrange(division) %>%
  slice_head(n = 1) %>%
  ungroup()

appearances_file <- here::here("input", "gowlt-WLCA-2023.html")
appearances_list <-
  rvest::read_html(appearances_file) %>%
  rvest::html_elements(xpath = "//div[@class='sectioner']") %>%
  rvest::html_elements(xpath = "//table") %>%
  html_table()
appearances <- appearances_list[[1]] %>%
  janitor::clean_names()

appearances_with_demographics <-
  appearances %>%
  inner_join(membership_by_rated_team, by = "name")

up_down_data <-
appearances_with_demographics %>%
  mutate(
    matches = wins + losses,
    win_ratio = wins / (matches)
  ) %>%
  mutate(move_up = case_when(
    line_avg <= 2.4 & win_ratio >= 0.7 ~ 1 & matches >= 4,
    TRUE ~ 0
  )) %>%
  mutate(move_down = case_when(
    line_avg >= 4.0 & win_ratio <= 0.2 ~ 1 & matches >= 4,
    TRUE ~ 0
  ))

rating_review <-
up_down_data %>%
  select(
    'First Name' = first,
    'Last Name' = last,
    Team = team,
    Club = location1,
    Rating = rating,
    W = wins,
    L =losses,
    'Win Ratio' = win_ratio,
    '# Matches' = matches,
    'Avg Court' = line_avg,
    move_up,
    move_down
  )

rating_review %>% write_csv(here::here("output", "rating_review.csv"))

