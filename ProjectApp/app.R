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


# functions -------------------------------------------------------------------------------------------------------------------------------

# used in graphics
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

# used in graphics
source_stats <- function(source1, source2) {
  data <- steamDataGenres %>% 
    select(any_of(c(source1, source2))) %>% 
    drop_na(any_of(c(source1, source2)))
  
  mod <- lm(as.formula(str_c(source1, " ~ ", source2)), data)
  
  stats <- list(
    source1 = source1,
    source2 = source2,
    coef = coef(mod)[2],
    max = confint(mod)[source2, "97.5 %"],
    min = confint(mod)[source2, "2.5 %"],
    cor = cor(data)[source2, source1]
  )
  
  return(stats)
}


# functions end -----------------------------------------------------------------------------------------------------------------------------


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
      tabPanel("Explore Price to User rating",
               
               uiOutput("plot_page")
      ),
      
      # TAB3 -----------------------------------------------------------------------------------------------------------------------------------
      tabPanel("Examining Price Distribution",
               
               uiOutput("density_page")
               
      )
      
      # END TABS --------------------------------------------------------------------------------------------------------------------------------
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #----------------TAB1---------------------------------------------------------------------------------------------------------------------------
  
  
  # server logic to handle UI design of introduction page
  output$info_paragraph <- renderUI({
    fluidRow(
      
      #HTML style code for formatting the information page to introduce project to user
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
               
               plotOutput('price_dense_intro'),
               
               plotOutput('priceToYear'),
               
               plotOutput('correlations'),
               
               plotOutput('score_ave'),
               
               tags$h3("General Findings"),
               tags$p("TBD")
            )
         )
      )
  })
  
  
  # server logic for summarising plot of price to userscore
  output$big_price_plotO <- renderPlot({
    
    # setup and vector creation
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
    
    # create plot for pop price genre used to summarise price to user score
    plot <- pop_price_genre %>% 
      mutate(genre = factor(genre, levels = c("Racing", "Adventure", "Casual", "Indie", "RPG", "Action", "Simulation"))) %>% 
      ggplot() +
      geom_bar(aes(x = genre, y = coef, fill = source, color = source), position = "dodge", alpha = 0.5, stat = "identity") +
      geom_errorbar(aes(x = genre, ymin = min, ymax = max, color = source), position = "dodge") +
      theme_classic()
    
    # plot call
    plot
    
  })
  
  
  # server logic for price density plot for info page
  output$price_dense_intro <- renderPlot({
    
    # create plot of price density for info page
    plotto <- steamDataClean %>% 
      ggplot() +
      geom_density(aes(x = full_price), fill = "#C5C3C0") +
      labs(x = "Price of Game (USD, excluding discounts)", y = "Proportion of Games") +
      theme_classic()
    
    # plot
    plotto
  })
  
  
  # server logic for price to year plot
  output$priceToYear <- renderPlot({
    
    # data manipulation and setup
    avg_price <- steamDataClean %>%  
      mutate(published_store = ymd(published_store)) %>%
      mutate(year = year(published_store)) %>% 
      group_by(year) %>% 
      mutate(avg_price = mean(full_price), cases = n()) %>% 
      filter(year > 2009)
    
    # create price to year plot
    plottski <- ggplot(avg_price) +
      geom_bar(aes(x = year, y = avg_price/cases, fill = cases), stat = "identity") +
      #geom_text(aes(x = year, y = avg_price, label = cases), vjust = -.5) +
      labs(y = "Average Price (USD)", fill = "# Games Published", x = "Year Published") +
      scale_fill_gradient(high = "#4C6B22", low = "#D2E885") +
      theme_classic()
    
    # call to plot
    plottski
  })
  
  
  # server logic for correlation plot output
  output$correlations <- renderPlot({
    
    # data manipulation for correlation plot
    correlations <- steamDataGenres %>% 
      select("igdb_score", "meta_score", "igdb_uscore", "meta_uscore", "store_uscore", "gfq_rating") %>%
      rename("IGDB" = igdb_score, 
             "Metacritic" = meta_score, 
             "IGDB Users" = igdb_uscore, 
             "Metacritic Users" = meta_uscore, 
             "Steam Users" = store_uscore,
             "GameFAQs" = gfq_rating) %>% 
      drop_na() %>% 
      cor()
    
    # create correlation matrix plot
    plotaroo <- corrplot(correlations, method = "number", order = "hclust", tl.col = "black", tl.srt = 45)
    
    # call to plot
    plotaroo
  })
  

  # server logic for creating score averages plot
  output$score_ave <- renderPlot({
    
    # data manipulation and setup for plot
    score_avg <- steamDataGenres %>%
      pivot_longer(cols = c(meta_score, meta_uscore, igdb_score, igdb_uscore), names_to = "score_name") %>% 
      group_by(score_name) %>% 
      summarize(avg = mean(value, na.rm = TRUE)) %>% 
      mutate(score_type = ifelse(str_detect(score_name, "uscore"), "Critic Rating", "User Rating")) %>% 
      mutate(source = ifelse(str_detect(score_name, "igdb"), "IGDB", "Metacritic"))
    
    # create plot for score averages
    plotskittles <- ggplot(score_avg) +
      geom_bar(aes(x = score_name, y = avg, fill = score_type), stat = "identity") +
      facet_wrap(~source, scales = "free_x") +
      labs(y = "Average Rating", x = "", fill = "Score Type") +
      theme_classic() +
      scale_fill_manual(values = c("#325974", "#67C1F5"))
    
    # call to plot
    plotskittles
  })
  
  #----------------TAB2--------------------------------------------------------------------------------------------------------------------------
  
  # UI output server logic for interactive price vs userscore page
  output$plot_page <- renderUI({
    fluidRow(
      
      #selection and slider inputs
      column(6,
             selectInput("rating_select", "Select rating", 
                         choices = c("Steam Userscore", 
                                     "IGDB Userscore", 
                                     "Metacritic Userscore")),
             selectInput("genre_select", "Select game genre", 
                         choices = c("All",
                                     "Adventure", 
                                     "Casual", 
                                     "Action",
                                     "Racing",
                                     "Indie",
                                     "RPG",
                                     "Simulation")),
             sliderInput("price_select",
                         "Price parameter",
                         min = 0,
                         max = max(steamDataGenres$full_price),
                         value = 50)
      ),
      
      #HTML style objects for user directions
      column(6,
             tags$div(
               style = "font-family: Arial, sans-serif; line-height: 1.5;",
               
               tags$h3("Directions of Usage"),
               tags$p("Use this page to explore the pricing of video games on Steam vs their user rating. You can use the selector tools
             on the lefthand side of the page to change which user ratings (from 3 different websites) to view, which genre of game you 
             want to examine, and the pricing value that you want to limit the graph to.")
             ),
      ),
      
      # calls to plot outputs for price to userscore page
      column(12,
             plotOutput("price_to_rating"),
             plotOutput("big_price_plot")
             )
    )
  })
  
  
  # plot output server logic to handle interactive price vs userscore plot
  output$price_to_rating <- renderPlot({
    
    # handles userscore user input choice selection bar
    titleholder = ""
    if (input$rating_select == "Steam Userscore") {
      titleholder = "store_uscore"
    } else if (input$rating_select == "IGDB Userscore") {
      titleholder = "igdb_uscore"
    } else {
      titleholder = "meta_uscore"
    }
    
    # declare plot
    plot <- NULL
    
    # if statement to handle in case user selects All genres
    if (input$genre_select == "All") {
      
      cleanData <- steamDataGenres %>% 
        select(full_price, !!sym(titleholder)) %>%
        filter(full_price <= input$price_select, !is.na(!!sym(titleholder) <= 250))

    } else {
      
      cleanData <- steamDataGenres %>% 
        select(full_price, !!sym(input$genre_select), !!sym(titleholder)) %>%
        filter(!is.na(!!sym(input$genre_select)), full_price <= input$price_select, !!sym(titleholder) <= 250)
    }
    
    # create plot based on user input for price vs userscore of choice
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
  
  
  # informative graph summarizing price vs userscore
  output$big_price_plot <- renderPlot({
    
    # setup for viz
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
    
    # plot
    plot <- pop_price_genre %>% 
      mutate(genre = factor(genre, levels = c("Racing", "Adventure", "Casual", "Indie", "RPG", "Action", "Simulation"))) %>% 
      ggplot() +
      geom_bar(aes(x = genre, y = coef, fill = source, color = source), position = "dodge", alpha = 0.5, stat = "identity") +
      geom_errorbar(aes(x = genre, ymin = min, ymax = max, color = source), position = "dodge") +
      theme_classic()
    
    plot
  })
  
  
  #----------------TAB3---------------------------------------------------------------------------------------------------------------------------
  
  
  # price density user interactive graph UI server definition
  output$density_page <- renderUI({
    fluidRow(
      column(6,
             
             #inputs for user design
             selectInput("rating_selectD", "Select rating", 
                         choices = c("Steam Userscore", 
                                     "IGDB Userscore", 
                                     "Metacritic Userscore")),
             selectInput("genre_selectD", "Select game genre", 
                         choices = c("All",
                                     "Adventure", 
                                     "Casual", 
                                     "Action",
                                     "Racing",
                                     "Indie",
                                     "RPG",
                                     "Simulation")),
             sliderInput("price_selectD",
                         "Price parameter",
                         min = 0,
                         max = max(steamDataGenres$full_price),
                         value = 35),
             sliderInput("pop_max", 
                         "Max Popularity",
                         min = 1, 
                         max = 100,
                         value = 100),
             sliderInput("pop_min",
                         "Min Popularity",
                         min = 0,
                         max = 99,
                         value = 0)
             ),
      
      #HTML style logic for user directions
      column(6,
             tags$div(
               style = "font-family: Arial, sans-serif; line-height: 1.5;",
               
               tags$h3("Directions of Usage"),
               tags$p("Use this page to explore the density of pricing of video games on Steam. You can use the selector tools
               on the lefthand side of the page to change which user ratings (from 3 different websites) to view, which genre of game you 
               want to examine, and the pricing value that you want to limit the graph to. You can also select the popularity range 
               you would like to examine.")
             ),
      ),
      
      # call to price density plot
      column(12,
             plotOutput("price_density")
            )
      )
  })
  
  
  # price density user interactive graph server definition
  output$price_density <- renderPlot({
    
    # handles userscore user input setting
    titleholderD = ""
    if (input$rating_selectD == "Steam Userscore") {
      titleholderD = "store_uscore"
    } else if (input$rating_selectD == "IGDB Userscore") {
      titleholderD = "igdb_uscore"
    } else {
      titleholderD = "meta_uscore"
    }
    
    #handles if genre select is set to all
    if (input$genre_selectD == "All") {
      cleanData <- steamDataGenres %>% 
        select(full_price, !!sym(titleholderD)) %>%
        filter(full_price <= input$price_selectD,
               !!sym(titleholderD) <= input$pop_max,
               !!sym(titleholderD) >= input$pop_min)
    } else {
      # handles genre setting and min/max setting
      cleanData <- steamDataGenres %>% 
        select(full_price, !!sym(input$genre_selectD), !!sym(titleholderD)) %>%
        filter(!is.na(!!sym(input$genre_selectD)), 
               full_price <= input$price_selectD,
               !!sym(titleholderD) <= input$pop_max,
               !!sym(titleholderD) >= input$pop_min)
    }
    
    # plot is plotted using the set settings
    plotski <- ggplot(cleanData) + 
      geom_density(aes(x = full_price)) +
      labs(title = str_c("Price density of ", as.character(input$genre_selectD), " Steam Games, in terms of ", as.character(input$rating_selectD)), x = "Full Price of Video Games", y = "Density") +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom")
    
    # call to plot
    plotski
  })
  
  
  # END SERVER LOGIC ---------------------------------------------------------------------------------------------------------------------------
  
}

# Run the application 
shinyApp(ui = ui, server = server)
