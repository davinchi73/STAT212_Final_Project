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
      
      tabPanel("Info Page",
               
               fluidRow(
                 column(12, 
                        verbatimTextOutput("info_paragraph")
                 )
               )
        
      ),
      
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
               
               sliderInput("price_select",
                           "Price parameter",
                           min = 0,
                           max = 70,
                           value = 35),
               
               plotOutput("price_to_rating")
      )
      
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$info_paragraph <- renderText({
    "- Our project is focused on one of the largest sources of creative output of the 21st century: video games. 
     Video games frequently employ detailed narratives, sophisticated and challenging gameplay, and can connect 
     with profound elements of the human psyche. We’ve decided to look at trends in video games as a way to
     assess what draws people to particular games.

    - In large part, we’ve kept our exploration open-ended, but we’ve focused on the factors that influence the 
    ratings and popularity of video games on Steam, one of the largest platforms for purchasing and playing video 
    games, with an estimated 60% of total PC game revenue captured by the platform.* 
    
    - We first were interested in any possible relationship between price and developer size. At first 
    glance, however, a clear relationship is not apparent. However, it is interesting to note that there are relatively 
    few large developers (measured by the number of Steam games published), which may warrant further investigation. 
    It might be worthwhile to look at large developers and small developers separately.
    
    - Diving in deeper to the data, we decided to investigate price vs user ratings across three different websites. 
    We chose to look closely at the ratings from users on Steam (of course), IGDB, and Metacritic. Each of these 
    websites are highly active and have a large number of users. Our rationale was that different categories of games 
    may inherently have higher development costs, or may  be subject to different kinds of pressures from their users. 
    Overall, we found a weak, positive relationship between price and popularity. One possibility for this relationship 
    existing at all, however, is the possibility that certain reviews might be manipulated. Developers of higher priced games 
    might be under pressure to showcase a polished product, and for that reason might have bots add positive reviews of the product. 
    This is one limitation of our analysis.
    
    - To go even further into our investigation, we decided to see if there exists any variation between genres for each of these 
    three websites’ user ratings. For sake of eliminating edge cases, we decided to ignore any games that are more than $70, as 
    there are very few. After plotting and examining our results, we found that there is little correlation between user ratings 
    and price of each video game. It looked like the ratings were more variable when observing the results from the Steam user
    ratings, and there was less correlation between price and user scores when looking at either Metacritic or IGDB. This is 
    somewhat interesting due to the fact that Steam is the most popular of the three websites and this would hypothetically lead
    to more watered-down results. This might be because of manipulation, since Steam is the actual platform where users buy games 
    and it would be more advantageous for developers to boost ratings on Steam. Due to our lack of significant findings in our 
    research so far, we hope to locate a dataset or more information, directly from the Steam API, that will help us discover 
    more about how the size of developers influences user ratings, and also how the user ratings of cheap or expensive games 
    change over time.
    "
  })
  
  #----------------TAB1---------------------------------------------------------------------------------------------------------------------------
  
  # Generate plot TAB 1 based on user selection
  output$explore_plots <- renderPlot({
    plot <- NULL
    
    # Check user selection and generate plot accordingly
    if (input$plot_select == "Density Plot of Prices") {
      
      plot <- ggplot(steamDataClean) +
        geom_density(aes(x = full_price)) +
        theme_classic() +
        theme(plot.margin = margin(5.5, 5.5, 150, 5.5))
      
      plot <- ggdraw() +
        draw_plot(plot) +
        draw_label("THIS WILL BE A PARAGRAPH EXPLAINING RESULTS ON THE PLOT", 
                   x = 0.5, y = -0.001, size = 12, hjust = 0.5, vjust = -8)
      
    } else if (input$plot_select == "Developer Size vs Price") {
      
      plot <- ggplot(steamDataDevelopers) +
        geom_point(aes(x = developer_size, y = full_price)) +
        geom_smooth(aes(x = developer_size, y = full_price), method = "lm", se = FALSE) +
        theme_classic() +
        labs(y = "Price", x = "Developer Size") +
        theme(plot.margin = margin(5.5, 5.5, 150, 5.5))
      
      plot <- ggdraw() +
        draw_plot(plot) +
        draw_label("THIS WILL BE A PARAGRAPH EXPLAINING RESULTS ON THE PLOT", 
                   x = 0.5, y = -0.001, size = 12, hjust = 0.5, vjust = -8)
      
    } else if (input$plot_select == "Average Price Over Years") {
      
      plot <- steamDataClean %>%  
        mutate(published_store = ymd(published_store)) %>%
        mutate(year = year(published_store)) %>% 
        group_by(year) %>% 
        summarize(avg_price = mean(full_price)) %>%
        ggplot() +
        geom_bar(aes(x = year, y = avg_price), stat = "identity") +
        theme(plot.margin = margin(5.5, 5.5, 150, 5.5))
      
      plot <- ggdraw() +
        draw_plot(plot) +
        draw_label("THIS WILL BE A PARAGRAPH EXPLAINING RESULTS ON THE PLOT", 
                   x = 0.5, y = -0.001, size = 12, hjust = 0.5, vjust = -8)
      
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
      filter(!is.na(!!sym(input$genre_select)), full_price <= input$price_select, !!sym(titleholder) <= 250)
    
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
