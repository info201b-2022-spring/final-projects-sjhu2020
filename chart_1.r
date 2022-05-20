library("dplyr")
library("wordcloud")

games_df <- read.csv("data/Games.csv")

genres_df <- select(games_df, Genre)

genres_grouped_df <- group_by(genres_df, Genre)

genres_grouped_freq_df <- summarize(genres_grouped_df, Total = length(Genre))

wordcloud(genres_grouped_freq_df$Genre , genres_grouped_freq_df$Total , col=terrain.colors(length(genre_list) , alpha=0.9) , rot.per=0.3 )


