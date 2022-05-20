library(tidyverse)
library(lubridate)
library(reshape2)

file3 <- read_csv(file = "data/multiTimeline.csv")
melted_df <- melt(file3, id = "dates")
dates <- as.Date(file3$dates)
date_months <- month(dates)
processed_df <- data.frame(dates ,melted_df)
new_df <- left_join(file3, processed_df) 

genre_trend_line <- ggplot(melted_df, aes(x = dates, y = value, color = variable)) + 
  geom_line() 

