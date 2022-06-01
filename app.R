library(shiny)
library(dplyr)
library(fmsb)
library(wordcloud)
library(lubridate)
library(reshape2)

char_df <- read.csv("data/characters.csv")
timeline_df <- read.csv("data/multiTimeline.csv")
games_df <- read.csv("data/Games.csv", encoding = "UTF-8")

summary_page <- tabPanel( # initializing our pages as variables will make organization much easier
  "Summary", # text(a title) is required at minimum, cannot be empty
  titlePanel("Examining characters in Mario Kart"),
  p("This visualiation lets you examine various characters in the game.")
  
) 

analysis_page <- tabPanel(
  "Analysis",
  #selectInput( # our old code from last lecture!
  #  inputId = "char",
  #  label = "Select a character!",
  #  choices = char_df$Character
  #),
  #tableOutput(outputId = 'table'),
  #plotOutput(outputId = 'radar')
  sidebarLayout(
    sidebarPanel( # some reorganizing
      h3("Control Panel"),
      selectInput(
        inputId = "char",
        label = "Select a character!",
        choices = char_df$Character
      ),
      
    ),
    mainPanel(
      plotOutput(outputId = 'radar'),
      tableOutput(outputId = 'table')
    )
  )
) 

cloud_page<- tabPanel(
  "Word Cloud Page",
  plotOutput(outputId = 'cloud')
)

genre_trend <- tabPanel(
  "Genre Trend",
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
    )
)

# Define UI for application that draws a histogram
ui <- navbarPage( # to have multiple pages use navbar instead of fluid
  "MarioKart Demo",
  summary_page,
  analysis_page,
  cloud_page,
  genre_trend
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  make_radar_df <- function(char_name) {
    rd_df <- select(char_df, -c(Character, Class))
    
    # we need the max and min of all features for a radar chart to work
    min_df <- summarize_all(rd_df, min)
    max_df <- summarize_all(rd_df, max)
    
    data_pt <- filter(char_df, Character == char_name)
    data_pt <- select(data_pt, -c(Character, Class))
    
    # lets glue all the data frames together (like a lasagna)
    return(do.call("rbind", list(max_df, min_df, data_pt)))
  }
  
  output$table <- renderTable(
    {
      #return(filter(char_df, Character == input$char))
      return(make_radar_df(input$char))
    }
  )
  
  output$radar <- renderPlot(
    {
      radarchart(make_radar_df(input$char))
    }
  )
  
  output$cloud <- renderPlot(
    {
      genres_df <- select(games_df, Genre)
      genres_grouped_df <- group_by(genres_df, Genre)
      genres_grouped_freq_df <- summarize(genres_grouped_df, Total = length(Genre))
      wordcloud(genres_grouped_freq_df$Genre , genres_grouped_freq_df$Total ,  alpha=0.9 , rot.per=0.3 )
    }
  )
  
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
}

# Run the application 
shinyApp(ui = ui, server = server)
