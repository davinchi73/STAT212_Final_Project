#
# This is a Shiny web application created by: Chris and Devinn (AKA, the RStudio Bad Boys)
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

# functions
genre_stats <- function(metric, source, genre) {
  data <- steamDataGenres %>%
    filter(genre == genre) %>%
    drop_na(any_of(c(genre, source, metric))) %>% 
    select(any_of(c(source, metric)))
  
  mod <- lm(as.formula(str_c(metric, " ~ ", source)), data)
  
  stats <- list(
    genre = genre,
    source = source,
    coef = coef(mod)[2],
    max = confint(mod)[source, "97.5 %"],
    min = confint(mod)[source, "2.5 %"],
    cor = cor(data)[source, metric]
  )
  
  return(stats)
}

# ui design start here
ui <- fluidPage(

    # Application title
    titlePanel("Steam Data Exploration"),
    
    # Tab layout
    tabsetPanel(
      
      # TAB1 -------------------------------------------------------------------------------------------------------------------------------------
      tabPanel("Info Page", 
               
               uiOutput("info_paragraph")
      ),
      
      # TAB2 -----------------------------------------------------------------------------------------------------------------------------------
      
      # tabPanel("User Guide",
      # 
      #         uiOutput("directions_page")
      # 
      # ),
      
      # TAB3 -----------------------------------------------------------------------------------------------------------------------------------
      
      # tabPanel("Explore Steam Data",
      #          
      #          # Plot select (switch between plots)
      #          selectInput("plot_select", "Select Plot", 
      #                      choices = c("Density Plot of Prices", 
      #                                  "Developer Size vs Price", 
      #                                  "Average Price Over Years")),
      #          
      #          # plot
      #          plotOutput("explore_plots")
      #          ),
      
      # TAB4 -----------------------------------------------------------------------------------------------------------------------------------
      tabPanel("Explore Price to User rating",
               
               # # Rating select (switch between specific rating scale)
               # selectInput("rating_select", "Select rating", 
               #             choices = c("Steam Userscore", 
               #                         "IGDB Userscore", 
               #                         "Metacritic Userscore")),
               # 
               # # Genre select (switch between most popular game genres)
               # selectInput("genre_select", "Select game genre", 
               #             choices = c("Adventure", 
               #                         "Casual", 
               #                         "Action",
               #                         "Racing",
               #                         "Indie",
               #                         "RPG",
               #                         "Simulation")),
               # 
               # # Price select (change the max price value on x axis to see large scale vs small scale shifts)
               # sliderInput("price_select",
               #             "Price parameter",
               #             min = 0,
               #             max = 70,
               #             value = 35),
               
               # plot
               uiOutput("plot_page")
      ),
      
      # TAB5 -----------------------------------------------------------------------------------------------------------------------------------
      
      tabPanel("Limitations and Future Implementations",
               
               uiOutput("limitations_page")
               
      )
      
      # END TABS --------------------------------------------------------------------------------------------------------------------------------
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #----------------TAB1---------------------------------------------------------------------------------------------------------------------------
  
  output$info_paragraph <- renderUI({
    fluidRow(
      column(12, 
             tags$div(
               style = "font-family: Arial, sans-serif; line-height: 1.5;",
               
               tags$h2("About Our Project"),
               
               tags$p("Our project is focused on one of the largest sources of creative output of the 21st century: video games. 
                    Video games frequently employ detailed narratives, sophisticated and challenging gameplay, and can connect 
                    with profound elements of the human psyche. We’ve decided to look at trends in video games as a way to
                    assess what draws people to particular games."),
               
               tags$p("In large part, we’ve kept our exploration open-ended, but we’ve focused on the factors that influence the 
                    ratings and popularity of video games on Steam, one of the largest platforms for purchasing and playing video 
                    games, with an estimated 60% of total PC game revenue captured by the platform.* "),
               
               tags$hr(),
               
               tags$h2("Key Findings"),
               
               plotOutput("big_price_plotO"),
               
               tags$h3("Price vs. User Ratings"),
               tags$p("Diving in deeper to the data, we decided to investigate price vs user ratings across three different websites. 
                    We chose to look closely at the ratings from users on Steam (of course), IGDB, and Metacritic. Each of these 
                    websites are highly active and have a large number of users. Our rationale was that different categories of games 
                    may inherently have higher development costs, or may  be subject to different kinds of pressures from their users. 
                    Overall, we found an extremely weak, and positive, relationship between price and popularity."),
               
               tags$h3("Genre Variations"),
               tags$p("To go even further into our investigation, we decided to see if there exists any variation between genres for each of these 
                    three websites’ user ratings. For sake of eliminating edge cases, we decided to ignore any games that are more than $70, as 
                    there are very few. After plotting and examining our results, we found that there is little correlation between user ratings 
                    and price of each video game. It looked like the ratings were more variable when observing the results from the Steam user
                    ratings, and there was less correlation between price and user scores when looking at either Metacritic or IGDB. This is 
                    somewhat interesting due to the fact that Steam is the most popular of the three websites and this would hypothetically lead
                    to more watered-down results. This might be because of manipulation, since Steam is the actual platform where users buy games 
                    and it would be more advantageous for developers to boost ratings on Steam. Due to our lack of significant findings in our 
                    research so far, we hope to locate a dataset or more information, directly from the Steam API, that will help us discover 
                    more about how the size of developers influences user ratings, and also how the user ratings of cheap or expensive games 
                    change over time."),
               
               tags$h3("General Findings"),
               tags$p("TBD")
            )
         )
      )
  })
  
  output$big_price_plotO <- renderPlot({
    
    genres <- c("Action", "Adventure", "Casual", "Racing", "Indie", "RPG", "Simulation")
    sources <- c("igdb_uscore", "meta_uscore", "store_uscore")
    inputs <- expand_grid(genres, sources)
    
    pop_price_genre <- map2(inputs$sources, inputs$genres, genre_stats, metric = "full_price") %>% 
      bind_rows()
    
    # Determines the order of genres based on average cor
    pop_price_genre %>% 
      group_by(genre) %>% 
      summarize(avg = mean(cor)) %>% 
      arrange(avg)
    
    plot <- pop_price_genre %>% 
      mutate(genre = factor(genre, levels = c("Racing", "Adventure", "Casual", "Indie", "RPG", "Action", "Simulation"))) %>% 
      ggplot() +
      geom_bar(aes(x = genre, y = coef, fill = source, color = source), position = "dodge", alpha = 0.5, stat = "identity") +
      geom_errorbar(aes(x = genre, ymin = min, ymax = max, color = source), position = "dodge") +
      theme_classic()
    
    plot
    
  })
  
  #----------------TAB2--------------------------------------------------------------------------------------------------------------------------
  
  # output$directions_page <- renderUI({ 
  #     fluidRow(
  #       column(12, 
  #              tags$div(
  #                style = "font-family: Arial, sans-serif; line-height: 1.5;",
  #                
  #                tags$h2("How to Use Our Tool"),
  #                
  #                # tags$h3("Explore Steam Data page"),
  #                # tags$p("Use this page to explore various aspects of the Steam API dataset. By using the dropdown selector tool on the 
  #                # lefthand side of the page, you can choose to view either a density plot of prices of video games on Steam, a plot that
  #                # displays game developer size vs price of games on Steam, or a plot that displays the average price of games on Steam over 
  #                # time."),
  #                
  #                tags$h3("Explore Price to User Rating Page"),
  #                tags$p("Use this page to explore the pricing of video games on Steam vs their user rating. You can use the selector tools
  #                on the lefthand side of the page to change which user ratings (from 3 different websites) to view, which genre of game you 
  #                want to examine, and the pricing value that you want to limit the graph to.")
  #                
  #                # tags$img(src = "../images/SLOGO.png", width = "100%", height = "auto") #not working
  #                
  #              ))
  #     )
  #   })
  
  #----------------TAB3---------------------------------------------------------------------------------------------------------------------------
  
  # # Generate plot TAB 2 based on user selection
  # output$explore_plots <- renderPlot({
  #   plot <- NULL
  #   
  #   # Check user selection and generate plot accordingly
  #   if (input$plot_select == "Density Plot of Prices") {
  #     
  #     plot <- ggplot(steamDataClean) +
  #       geom_density(aes(x = full_price)) +
  #       theme_classic() +
  #       theme(plot.margin = margin(5.5, 5.5, 150, 5.5))
  #     
  #     plot <- ggdraw() +
  #       draw_plot(plot) +
  #       draw_label("THIS WILL BE A PARAGRAPH EXPLAINING RESULTS ON THE PLOT", 
  #                  x = 0.5, y = -0.001, size = 12, hjust = 0.5, vjust = -8)
  #     
  #   } else if (input$plot_select == "Developer Size vs Price") {
  #     
  #     plot <- ggplot(steamDataDevelopers) +
  #       geom_point(aes(x = developer_size, y = full_price)) +
  #       geom_smooth(aes(x = developer_size, y = full_price), method = "lm", se = FALSE) +
  #       theme_classic() +
  #       labs(y = "Price", x = "Developer Size") +
  #       theme(plot.margin = margin(5.5, 5.5, 150, 5.5))
  #     
  #     plot <- ggdraw() +
  #       draw_plot(plot) +
  #       draw_label("THIS WILL BE A PARAGRAPH EXPLAINING RESULTS ON THE PLOT", 
  #                  x = 0.5, y = -0.001, size = 12, hjust = 0.5, vjust = -8)
  #     
  #   } else if (input$plot_select == "Average Price Over Years") {
  #     
  #     plot <- steamDataClean %>%  
  #       mutate(published_store = ymd(published_store)) %>%
  #       mutate(year = year(published_store)) %>% 
  #       group_by(year) %>% 
  #       summarize(avg_price = mean(full_price)) %>%
  #       ggplot() +
  #       geom_bar(aes(x = year, y = avg_price), stat = "identity") +
  #       theme(plot.margin = margin(5.5, 5.5, 150, 5.5))
  #     
  #     plot <- ggdraw() +
  #       draw_plot(plot) +
  #       draw_label("THIS WILL BE A PARAGRAPH EXPLAINING RESULTS ON THE PLOT", 
  #                  x = 0.5, y = -0.001, size = 12, hjust = 0.5, vjust = -8)
  #     
  #   }
  #   
  #   # Return the plot
  #   plot
  # })
  
  #----------------TAB4---------------------------------------------------------------------------------------------------------------------------
  
  
  output$plot_page <- renderUI({
    fluidRow(
      column(6,
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
                         value = 35)
      ),
      column(6,
             tags$div(
               style = "font-family: Arial, sans-serif; line-height: 1.5;",
               
               tags$h3("Directions of Usage"),
               tags$p("Use this page to explore the pricing of video games on Steam vs their user rating. You can use the selector tools
             on the lefthand side of the page to change which user ratings (from 3 different websites) to view, which genre of game you 
             want to examine, and the pricing value that you want to limit the graph to.")
             ),
      ),
      column(12,
             plotOutput("price_to_rating"),
             plotOutput("big_price_plot")
             )
    )
  })
  
  # Generate plot TAB 3 based on user selection
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
  
  
  output$big_price_plot <- renderPlot({
    
    genres <- c("Action", "Adventure", "Casual", "Racing", "Indie", "RPG", "Simulation")
    sources <- c("igdb_uscore", "meta_uscore", "store_uscore")
    inputs <- expand_grid(genres, sources)
    
    pop_price_genre <- map2(inputs$sources, inputs$genres, genre_stats, metric = "full_price") %>% 
      bind_rows()
    
    # Determines the order of genres based on average cor
    pop_price_genre %>% 
      group_by(genre) %>% 
      summarize(avg = mean(cor)) %>% 
      arrange(avg)
    
    plot <- pop_price_genre %>% 
      mutate(genre = factor(genre, levels = c("Racing", "Adventure", "Casual", "Indie", "RPG", "Action", "Simulation"))) %>% 
      ggplot() +
      geom_bar(aes(x = genre, y = coef, fill = source, color = source), position = "dodge", alpha = 0.5, stat = "identity") +
      geom_errorbar(aes(x = genre, ymin = min, ymax = max, color = source), position = "dodge") +
      theme_classic()
    
    plot
    
  })
  
  #----------------TAB5---------------------------------------------------------------------------------------------------------------------------
  
  output$limitations_page <- renderUI({
    
    fluidRow(
      column(12, 
             tags$div(
               style = "font-family: Arial, sans-serif; line-height: 1.5;",
               
               tags$h3("Limitations to Our Research"),
               tags$p("TBD"),
               
               tags$h3("Possible Future Implementations"),
               tags$p("TBD")
               
             ))
    )
    
  })
  
  # END SERVER LOGIC ---------------------------------------------------------------------------------------------------------------------------
  
}

# Run the application 
shinyApp(ui = ui, server = server)
