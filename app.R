library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

video_game_data = read_csv("Video_Games.csv")

#remove rows where genre NA
video_game_data = video_game_data %>%
  filter(!is.na(Genre))

#UI
ui = fluidPage(
  titlePanel("Video games sales analysis"),
  
  #panel 1 w/ 2 tabs
  sidebarLayout(
    sidebarPanel(
      selectInput("genre", "Select genre(s), otherwise defaults to show all", 
                  choices = unique(video_game_data$Genre), 
                  multiple = TRUE),
      
      sliderInput("year", "Select year of release:",
                  min = min(video_game_data$Year_of_Release, na.rm = TRUE),
                  max = max(video_game_data$Year_of_Release, na.rm = TRUE),
                  value = c(1980, 2020),
                  sep = "")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Global sales over years", plotOutput("salesPlot")),
        tabPanel("Sales by region", plotOutput("regionSalesPlot"))
      )
    )
  ),
  
  titlePanel("Video games ratings analysis"),
  #panel 2 w/ 2 tabs
  sidebarLayout(
    sidebarPanel(
      selectInput("genre2", "Select genre(s), otherwise defaults to show all", 
                  choices = unique(video_game_data$Genre), 
                  multiple = TRUE),
      
      sliderInput("year2", "Select year of release:",
                  min = min(video_game_data$Year_of_Release, na.rm = TRUE),
                  max = max(video_game_data$Year_of_Release, na.rm = TRUE),
                  value = c(1980, 2020),
                  sep = "")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Critic scores by genre", plotOutput("scorePlot")),
        tabPanel("Critic vs user score", plotOutput("criticUserScorePlot"))
      )
    )
  )
)

#Server
server = function(input, output) {
  
  #filter data based on inputs from the first sidebar
  filtered_data1 <- reactive({
    if (length(input$genre) > 0) {
      video_game_data %>%
        filter(Genre %in% input$genre & Year_of_Release >= input$year[1] & Year_of_Release <= input$year[2])
    } else {
      video_game_data %>%
        filter(Year_of_Release >= input$year[1] & Year_of_Release <= input$year[2])
    }
  })
  
  #filter data based on inputs from the second sidebar
  filtered_data2 <- reactive({
    if (length(input$genre2) > 0) {
      video_game_data %>%
        filter(Genre %in% input$genre2 & Year_of_Release >= input$year2[1] & Year_of_Release <= input$year2[2])
    } else {
      video_game_data %>%
        filter(Year_of_Release >= input$year2[1] & Year_of_Release <= input$year2[2])
    }
  })
  
  #sales plot
  output$salesPlot <- renderPlot({
    sales_data <- filtered_data1() %>%
      group_by(Year_of_Release) %>%
      summarize(Global_Sales = sum(Global_Sales, na.rm = TRUE))
    
    ggplot(sales_data, aes(x = Year_of_Release, y = Global_Sales)) +
      geom_line(color = "blue") +
      labs(title = paste("Global sales for selected genre(s)"),
           x = "Year of release",
           y = "Global sales (in millions)") +
      theme_minimal()
  })
  
  #sales by region
  output$regionSalesPlot <- renderPlot({
    region_sales_data <- filtered_data1() %>%
      summarize(NA_Sales = sum(NA_Sales, na.rm = TRUE),
                EU_Sales = sum(EU_Sales, na.rm = TRUE),
                JP_Sales = sum(JP_Sales, na.rm = TRUE),
                Other_Sales = sum(Other_Sales, na.rm = TRUE)) %>%
      pivot_longer(cols = c(NA_Sales, EU_Sales, JP_Sales, Other_Sales), 
                   names_to = "Region", 
                   values_to = "Sales")
    
    ggplot(region_sales_data, aes(x = Region, y = Sales, fill = Region)) +
      geom_bar(stat = "identity") +
      labs(title = "Total sales by region",
           x = "Region",
           y = "Total sales (in millions)") +
      theme_minimal()
  })
  
  #critic score
  output$scorePlot <- renderPlot({
    score_data <- filtered_data2()
    
    ggplot(score_data, aes(x = Genre, y = Critic_Score, fill = Genre)) +
      geom_boxplot() +
      labs(title = "Distribution of critic scores by selected genre(s)",
           x = "Genre",
           y = "Critic score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #critic vs user score
  output$criticUserScorePlot <- renderPlot({
    score_data <- filtered_data2() %>%
      mutate(Critic_Score = suppressWarnings(as.numeric(Critic_Score)),
             User_Score = suppressWarnings(as.numeric(User_Score))) %>%
      filter(!is.na(Critic_Score) & !is.na(User_Score))  #only valid numeric scores for user and critic
    
    score_data <- score_data %>%
      mutate(User_Score = User_Score * 10)
    
    if (nrow(score_data) == 0) {
      return(NULL)  #not rendering plot if no valid data
    }
    
    ggplot(score_data, aes(x = Critic_Score, y = User_Score)) +
      geom_point(color = "darkred", alpha = 0.6) +
      geom_smooth(method = "lm", color = "blue", se = FALSE) +
      scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
      scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
      labs(title = "Critic score vs user score (both scaled to 100)",
           x = "Critic score (out of 100)",
           y = "User score (out of 100)") +
      theme_minimal()
  })
}

#run app
shinyApp(ui = ui, server = server)