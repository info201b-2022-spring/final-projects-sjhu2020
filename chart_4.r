library("dplyr")
library("wordcloud")
library("stringr")

games_df <- read.csv("data/Games.csv", encoding = "UTF-8")

year_long <- function(year_short) {
  year_short <- as.numeric(year_short)
  if (year_short < 84) {
    year_short <- 2000 + year_short
  } else {
    year_short <- 1900 + year_short
  }
}

games_df$Release <- lapply(str_replace_all(games_df$Release, '[^0-9]', ''), year_long)

#genres_df <- games_df
genres_df <- filter(games_df, Release <= 2001)

genres_grouped_df <- group_by(genres_df, Genre)

genres_grouped_freq_df <- summarize(genres_grouped_df, Total = length(Genre))

cloud <- wordcloud(genres_grouped_freq_df$Genre , genres_grouped_freq_df$Total, scale = c(2, 0.2))