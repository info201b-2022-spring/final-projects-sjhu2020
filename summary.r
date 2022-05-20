library("dplyr")

games_df <- read.csv("/Users/ron0821/Desktop/INFO 201/final-projects-sjhu2020/Games.csv")

summary_info <- list()

summary_info$num_observations <- nrow(games_df)
# There are total of 175 observations (games) in the dataset
summary_info$most_num_sales <- max(games_df$Sales)
# The number of most popular PC game sales is 42 million
summary_info$least_num_sales <- min(games_df$Sales)
# The number of least popular game sales is 1 million
summary_info$most_popular_game <- games_df[games_df$Sales == summary_info$most_num_sales, 'Name']
# This most popular PC game is PlayerUnknown's Battlegrounds
summary_info$most_popular_game_release_date <- games_df[games_df$Sales == summary_info$most_num_sales, 'Release']
# The release date of the most popular game (PlayerUnknown's Battlegrounds) was December, 2017



