library("dplyr")
library("wordcloud")

games_df <- read.csv("data/Games.csv", encoding = "UTF-8")

genres_df <- select(games_df, Genre)

genres_grouped_df <- group_by(genres_df, Genre)

genres_grouped_freq_df <- summarize(genres_grouped_df, Total = length(Genre))

cloud <- wordcloud(genres_grouped_freq_df$Genre , genres_grouped_freq_df$Total ,  alpha=0.9 , rot.per=0.3 )
# create word cloud displaying frequency of video game genres


