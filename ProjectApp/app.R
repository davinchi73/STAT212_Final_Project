#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# load packages here:
library(shiny)
library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)
library(skimr)
library(lubridate)
library(ggpubr)
library(cowplot)
library(broom)


# load data here:
steamData <- read_csv("../../data/data_clean/steam.csv")
steamDataClean <- read_csv("../../data/data_clean/steamDataClean.csv")
steamDataGenres <- read_csv("../../data/data_clean/steamDataGenres.csv")
steamDataDevelopers <- read_csv("../../data/data_clean/steamDataDevelopers.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Steam data"),
    
    # Tab layout
    tabsetPanel(
      
      tabPanel("Explore Steam Data",
               
               selectInput("plot_select", "Select Plot", 
                           choices = c("Density Plot of Prices", 
                                       "Developer Size vs Price", 
                                       "Average Price Over Years")),
               
               # Placeholder for plot output
               plotOutput("plot_output")
               ),
      
      tabPanel("TBD", "Empty"),
      tabPanel("TBD", "Empty")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Generate plot based on user selection
  output$plot_output <- renderPlot({
    plot <- NULL
    
    # Check user selection and generate plot accordingly
    if (input$plot_select == "Density Plot of Prices") {
      
      plot <- ggplot(steamDataClean) +
        geom_density(aes(x = full_price)) +
        theme_classic()
      
    } else if (input$plot_select == "Developer Size vs Price") {
      
      plot <- ggplot(steamDataDevelopers) +
        geom_point(aes(x = developer_size, y = full_price)) +
        geom_smooth(aes(x = developer_size, y = full_price), method = "lm", se = FALSE) +
        theme_classic() +
        labs(y = "Price", x = "Developer Size")
      
    } else if (input$plot_select == "Average Price Over Years") {
      
      plot <- steamDataClean %>%  
        mutate(published_store = ymd(published_store)) %>%
        mutate(year = year(published_store)) %>% 
        group_by(year) %>% 
        summarize(avg_price = mean(full_price)) %>%
        ggplot() +
        geom_bar(aes(x = year, y = avg_price), stat = "identity")
      
    }
    
    # Return the plot
    plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
