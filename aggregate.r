library("dplyr")
library(knitr)

games_df <- read.csv("data/Games.csv", encoding = "UTF-8")

genres_df <- select(games_df, Genre)

genres_grouped_df <- group_by(genres_df, Genre)

genres_grouped_freq_df <- summarize(genres_grouped_df, Total = length(Genre))
