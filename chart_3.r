library(tidyverse)
library(lubridate)
library(reshape2)

file3 <- read_csv(file = "data/multiTimeline.csv")
melted_df <- melt(file3, id = "dates")

genre_trendline <- ggplot(melted_df, aes(x = dates, y = value, color = variable)) + 
  geom_line() +
  labs(
    title = "Video Game Search Popularity by Genre",
    caption = "Source: Google Trends",
    color = "Video Game Genre",
    x = "Date From 2019 ~ Now, Collected Once A Week",
    y = "Number of Searches"
  )

genre_trendline