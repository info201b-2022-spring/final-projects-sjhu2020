library("dplyr")

games_df <- read.csv("data/Games.csv")
steam_games_df <- read.csv("data/steam_games.csv")
video_games_df <- read.csv("data/video_games.csv")

summary_info <- list()

summary_info$num_observations <- nrow(games_df)
summary_info$some_max_value <- games_df %>%
  filter(some_var == max(some_var, na.rm = T)) %>%
  select(some_label)