library(tidyverse)

file2 <- read_csv(file = "data/video_games.csv")

needed2 <- file2 %>% 
  select(Title, Metadata.Genres, `Features.Online?`, `Length.All PlayStyles.Average`) %>% 
    filter(Metadata.Genres %in% c("Action", "Adventure", "Simulation", "Sports", "Strategy"))

chart2 <- ggplot(needed2, 
                 aes(x = `Length.All PlayStyles.Average`)) +
  geom_histogram(fill = "blue", color = "red", alpha = 0.7)+
  facet_wrap(~Metadata.Genres) +
  labs(
    title = "Time Spend on Completing Based on Genre",
    subtitle = "Used The Five Major Genre",
    caption = "Source: video_games.csv",
    x = "Average Time Spent (Hr)",
    y = "Amount"
  )
  
chart2