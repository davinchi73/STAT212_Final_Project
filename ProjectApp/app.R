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
    titlePanel("General Steam data"),
    
    # Tab layout
    tabsetPanel(
      
      tabPanel("Explore Steam Data",
               
               selectInput("plot_select", "Select Plot", 
                           choices = c("Density Plot of Prices", 
                                       "Developer Size vs Price", 
                                       "Average Price Over Years")),
               
               plotOutput("explore_plots")
               ),
      
      tabPanel("Explore Price to User rating",
               
               selectInput("rating_select", "Select rating", 
                           choices = c("Steam Userscore", 
                                       "IGDB Userscore", 
                                       "Metacritic Userscore")),
               
               selectInput("genre_select", "Select game genre", 
                           choices = c("Adventure", 
                                       "Casual", 
                                       "Action",
                                       "Racing",
                                       "Indie",
                                       "RPG",
                                       "Simulation")),
               
               plotOutput("price_to_rating")
      ),
      
      tabPanel("TBD", "Empty")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #----------------TAB1---------------------------------------------------------------------------------------------------------------------------
  
  # Generate plot TAB 1 based on user selection
  output$explore_plots <- renderPlot({
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
  
  #----------------TAB2---------------------------------------------------------------------------------------------------------------------------
  
  # Generate plot TAB 2 based on user selection
  output$price_to_rating <- renderPlot({
    
    titleholder = ""
    if (input$rating_select == "Steam Userscore") {
      titleholder = "store_uscore"
    } else if (input$rating_select == "IGDB Userscore") {
      titleholder = "igdb_uscore"
    } else {
      titleholder = "meta_uscore"
    }
    
    plot <- NULL
    
    cleanData <- steamDataGenres %>% 
      select(full_price, !!sym(input$genre_select), !!sym(titleholder)) %>%
      filter(!is.na(!!sym(input$genre_select)), full_price <= 70, !!sym(titleholder) <= 250)
    
    plot <- ggplot(cleanData) + 
      geom_point(aes(x = full_price, y = !!sym(titleholder))) +
      stat_smooth(aes(x = full_price, y = !!sym(titleholder)), method = "lm", se = TRUE) +
      labs(title = str_c(as.character(input$rating_select), " score vs price for ", as.character(input$genre_select), " games"), 
           x = "Price", y = as.character(input$rating_select)) + 
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
    # Return the plot
    plot
  })
  
  #----------------TAB3---------------------------------------------------------------------------------------------------------------------------
  
}

# Run the application 
shinyApp(ui = ui, server = server)
