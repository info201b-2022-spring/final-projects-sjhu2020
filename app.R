library(shiny)
library(plotly)
library(dplyr)
library(fmsb)
library(wordcloud)
library(lubridate)
library(reshape2)
library(tidyverse)
library(knitr)
library(markdown)

timeline_df <- read.csv("data/multiTimeline.csv")
games_df <- read.csv("data/Games.csv", encoding = "UTF-8")

summary_page <- tabPanel( # initializing our pages as variables will make organization much easier
  "Summary", # text(a title) is required at minimum, cannot be empty
  titlePanel("Summary of our project"),
  includeMarkdown("summary.md")
) 

analysis_page <- tabPanel(
  "Introduction",
  includeMarkdown("intro.md")
) 

cloud_page<- tabPanel(
  "Word Cloud",
  includeMarkdown("chartinfo1.md"),
  sidebarPanel( # some reorganizing
    h3("Select Time"),
    sliderInput(
      inputId = "year",
      label = "Choose a Year",
      min = 1984,
      max = 2020,
      value = c(1984, 2020),
      sep = ""
    )
  ),
  
  mainPanel(
    plotOutput(outputId = 'cloud')
  ),
  dataTableOutput(outputId = "cloudTable")
)

time_spent <- tabPanel(
  "Time Spent",
  includeMarkdown("chartinfo2.md"),
  sidebarPanel( # some reorganizing
    h3("Select Genre"),
    selectInput(
      inputId = "genre",
      label = "Choose the Game Genre",
      choices = list("Action" , "Adventure", "Simulation", "Sports", "Strategy")
    ),
  ),
  
  mainPanel(
    plotlyOutput(outputId = 'hist')
  )
)

genre_trend <- tabPanel(
  "Genre Trend",
  includeMarkdown("chartinfo3.md"),
  sidebarPanel( # some reorganizing
    h3("Select Time"),
    sliderInput(
      inputId = "time",
      label = "Choose the Time Frame",
      min = as.Date("2019-01-06"),
      max = as.Date("2022-05-15"),
      value = c(as.Date("2019-01-06"), as.Date("2022-05-15"))
    ),
  ),
  
  mainPanel(
    plotOutput(outputId = 'line')
  ),
  dataTableOutput(outputId = "tableLine")
  
)

# Define UI for application that draws a histogram
ui <- navbarPage( # to have multiple pages use navbar instead of fluid
  "Video Game Analysis",
  includeCSS("styles.css"),
  analysis_page,
  cloud_page,
  time_spent,
  genre_trend,
  summary_page
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  output$line <- renderPlot(
    {
      file3 <- read_csv(file = "data/multiTimeline.csv")
      file3 <- file3 %>% filter(dates >= input$time[1])
      file3 <- file3 %>% filter(dates <= input$time[2])
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
      
      return(genre_trendline)
    }
  )
  
  output$tableLine <- renderDataTable(
    {
      file3 <- read_csv(file = "data/multiTimeline.csv")
      #file3 <- file3 %>% pull(date)
      file3 <- file3 %>% filter(dates >= input$time[1])
      file3 <- file3 %>% filter(dates <= input$time[2])
      return(file3)
    }
  )
  
  output$hist <- renderPlotly(
    {
      file2 <- read_csv(file = "data/video_games.csv")
      needed2 <- file2 %>% 
        select(Title, Metadata.Genres, `Features.Online?`, `Length.All PlayStyles.Average`) %>% 
        filter(Metadata.Genres == input$genre)
      needed2 <- needed2%>% 
        rename(
          `Avg Time Spent (Hr)` = `Length.All PlayStyles.Average`
        )
      genre_play_time <- ggplot(needed2, 
                                aes(x = `Avg Time Spent (Hr)`)) +
        geom_histogram(fill = "blue", color = "red", alpha = 0.7)+
        labs(
          title = "Time Spend on Completing Based on Genre",
          subtitle = "Used The Five Major Genre",
          caption = "Source: video_games.csv",
          x = "Average Time Spent (Hr)",
          y = "Amount"
        )
      newFig <- ggplotly(genre_play_time)
      return(newFig)
    }
  )
  
  output$cloud <- renderPlot(
    {
      year_long <- function(year_short) {
        year_short <- as.numeric(year_short)
        if (year_short < 84) {
          year_short <- 2000 + year_short
        } else {
          year_short <- 1900 + year_short
        }
      }
      games_df$Release <- lapply(str_replace_all(games_df$Release, '[^0-9]', ''), year_long)
      genres_df <- filter(games_df, Release >= input$year[1], Release <= input$year[2])
      genres_grouped_df <- group_by(genres_df, Genre)
      genres_grouped_freq_df <- summarize(genres_grouped_df, Total = length(Genre))
      cloud <- wordcloud(genres_grouped_freq_df$Genre , genres_grouped_freq_df$Total, scale = c(2, 0.1))
    }
  )
  
  output$cloudTable <- renderDataTable(
    {
      year_long <- function(year_short) {
        year_short <- as.numeric(year_short)
        if (year_short < 84) {
          year_short <- 2000 + year_short
        } else {
          year_short <- 1900 + year_short
        }
      }
      games_df$Release <- lapply(str_replace_all(games_df$Release, '[^0-9]', ''), year_long)
      genres_df <- filter(games_df, Release >= input$year[1], Release <= input$year[2])
      genres_grouped_df <- group_by(genres_df, Genre)
      genres_grouped_freq_df <- summarize(genres_grouped_df, Total = length(Genre))
      return(genres_grouped_freq_df)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
