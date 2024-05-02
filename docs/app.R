<<<<<<< HEAD
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
library(corrplot)


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
               
               ###
               tags$h2("About Our Project"),
               tags$p("Our project is focused on one of the largest sources of creative output of the 21st century: video games. 
                      Video games frequently employ detailed narratives, sophisticated and challenging gameplay, and can connect
                      with profound elements of the human psyche. We’ve decided to look at trends in video games as a way to assess 
                      what draws people to particular games."),
               
               tags$p("In large part, we kept our exploration open-ended, but we’ve focused on the factors that influence the ratings 
                      and popularity of video games on Steam, one of the largest platforms for purchasing and playing video games, 
                      with an estimated 60% of total PC game revenue captured by the platform. "),
               
               tags$p("In the end, we were able to draw a few interesting conclusions regarding trends in video games, but what struck 
                       us at the end was how difficult it was to find reliable data, especially for popularity metrics. We concluded that this 
                       is potentially a result of Steam’s strategy to maximize sales of video games."),
               
               
               tags$hr(),
               
               ###
               tags$h2("Limitations of Online Video Game Ratings"),
               tags$p("Our most striking finding ended up being that user ratings on third party websites, critic ratings on third party 
               websites, and user ratings on the Steam store did not match up very well."),
               
               plotOutput('correlations'),
               
               tags$p("We constructed a correlation matrix comparing each measure included in the dataset. The key takeaway here is 
               that, overall, rating websites often did not agree with each other when rating video games. The ratings for each video 
               game across different rating websites was positive in all cases; however, the relationship was weak in most cases. The 
               greatest correlation was between critics on IGDB and Metacritic, while the smallest was between IGDB critics and GameFAQ 
               users."),
               
               tags$p("These sites aim to aggregate ratings from many different users and critics in order to produce balanced ratings 
               of games, so it is surprising to see how little they actually line up with each other."),
               
               plotOutput('score_ave'),
               
               tags$p("This might be explained by differences in how critics and users rate games, so we compared mean critic and user 
               ratings on IGDB and Metacritic to investigate. In both cases, critics tended to rate games lower than users. Given this
               disparity, in addition to the fact that ratings across the board are only weakly related, a possible explanation is that 
               manipulation of ratings may occur for many games. Especially for games published by smaller developers that aren’t widely
               publicized, which make up a majority of cases in this dataset, flooding a game’s page with positive reviews would be a 
               relatively simple task. This is an intriguing possibility, but given time constraints, it’s not something we deeply 
               investigated."),
               
               tags$p("Given the limitations of popularity metrics from third-party sites as well as Steam itself, we looked into the 
                      possibility of using Steam’s API to obtain popularity metrics for games directly from the source. However, Steam 
                      does not provide a direct way to access these metrics, possibly as a way to avoid speculation that might negatively 
                      impact promotion of certain video games. Third parties did not have reliable metrics either, because of changes 
                      Steam has made to its API over the years."),
               
               tags$hr(),
               
               ###
               tags$h3("Price vs. User Ratings"),
               tags$p("Diving in deeper to the data, we decided to investigate price vs user ratings across three different websites.
               We chose to look closely at the ratings from users on Steam (of course), IGDB, and Metacritic. Each of these websites 
               are highly active and have a large number of users. Our rationale was that different categories of games may inherently have
               higher development costs, or may be subject to different kinds of pressures from their users. Overall, we found a weak, and 
               positive, relationship between price and popularity across categories. In the below plot, we plot how much each extra percentage
               point in rating translates to change in price."),
               
               plotOutput("big_price_plotO"),
               
               tags$p("For sake of eliminating edge cases, we decided to ignore any games that are more than $70, as there are very few. After 
                      plotting and examining our results, we found that there is little correlation between user ratings and price of each 
                      video game. Ratings tend to be less variable when observing the results from the Steam user ratings, and there was less
                      overall correlation between price and user scores when looking at either Metacritic or IGDB. In conjunction with our 
                      previous results, this might be because of manipulation, since Steam is the actual platform where users buy games and
                      it would be more advantageous for developers to boost ratings on Steam."),
               
               tags$hr(),
               
               ###
               tags$h2("How the Steam Store Changed Over Time"),
               tags$p("Steam was released relatively recently, which gives us a limited amount of data that we can draw general conclusions 
                      from, which would be taken into account. It seems as though Steam has been releasing more and more games steadily from
                      the period of its release to around 2020, where the number of games published seems to take a huge leap. This could be 
                      due to a number of reasons, however a likely reason could have been due to the Pandemic, COVID 19. This is a likely 
                      reason due to the dip in games released since that year, however it is difficult to say much definitively due to the 
                      lack of number of years since 2020."),
               
               plotOutput('priceToYear'),
               
               tags$p("When we examine price, it seems as though the lack of data available is quite limiting. We are unable to predict 
               meaningfully whether or not the average price or the number of games published per year will increase or decrease, however if 
               we continue to analyze considering COVID 19, we can say that it may have been likely that games published would have continued 
               to decrease until stabilization, and average price would likely have continued to fluctuate."),
      
               tags$hr(),
               
               ### 
               tags$h2("Ethics"),
               tags$p("Our investigation deals with factors that influence the price and overall sucess of video games. This affects three 
                      main players: video game developers, video game players, and middlemen like Steam. The data we found is gathered from
                      Steam, which has an incentive to increase sales and profits of the video games it sells. The possibility that Steam isn't
                      fully transparent with the data it shares must be considered. At the same, this data was gathered by a third-party source, 
                      which might have different incentives. One of the main problems with our investigation is that there is no reliable data 
                      on video game popularity that we could find. This makes sense - video game developers and Steam want to make their video
                      games seem like quality products that stand apart from other games. However, this ignores the vested interest of video game
                      players, who currently don't have a objective standard with which they can evaluate the quality of games before they buy
                      them."),
               
               tags$hr(),
               
               ###
               tags$h2("Data Context"),
               tags$p("This data was pulled from Steam’s API in 2022. Ratings were scraped from a number of different video game rating
                      websites. It represents all games available on the Steam store as of that date, including game’s name, price, genre
                      categories, ratings from third-party websites, and more. The data excludes games that are free to play."),
               
               plotOutput('price_dense_intro'),
               
               tags$p("We wanted to study factors that influence the ratings and popularity of video games on Steam, one of which included 
                      price. The majority of prices are less than $10 U.S. dollars.")
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
=======
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
library(corrplot)


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
               
               ###
               tags$h2("About Our Project"),
               tags$p("Our project is focused on one of the largest sources of creative output of the 21st century: video games. 
                      Video games frequently employ detailed narratives, sophisticated and challenging gameplay, and can connect
                      with profound elements of the human psyche. We’ve decided to look at trends in video games as a way to assess 
                      what draws people to particular games."),
               
               tags$p("In large part, we kept our exploration open-ended, but we’ve focused on the factors that influence the ratings 
                      and popularity of video games on Steam, one of the largest platforms for purchasing and playing video games, 
                      with an estimated 60% of total PC game revenue captured by the platform. "),
               
               tags$p("In the end, we were able to draw a few interesting conclusions regarding trends in video games, but what struck 
                       us at the end was how difficult it was to find reliable data, especially for popularity metrics. We concluded that this 
                       is potentially a result of Steam’s strategy to maximize sales of video games."),
               
               
               tags$hr(),
               
               ###
               tags$h2("Limitations of Online Video Game Ratings"),
               tags$p("Our most striking finding ended up being that user ratings on third party websites, critic ratings on third party 
               websites, and user ratings on the Steam store did not match up very well."),
               
               plotOutput('correlations'),
               
               tags$p("We constructed a correlation matrix comparing each measure included in the dataset. The key takeaway here is 
               that, overall, rating websites often did not agree with each other when rating video games. The ratings for each video 
               game across different rating websites was positive in all cases; however, the relationship was weak in most cases. The 
               greatest correlation was between critics on IGDB and Metacritic, while the smallest was between IGDB critics and GameFAQ 
               users."),
               
               tags$p("These sites aim to aggregate ratings from many different users and critics in order to produce balanced ratings 
               of games, so it is surprising to see how little they actually line up with each other."),
               
               plotOutput('score_ave'),
               
               tags$p("This might be explained by differences in how critics and users rate games, so we compared mean critic and user 
               ratings on IGDB and Metacritic to investigate. In both cases, critics tended to rate games lower than users. Given this
               disparity, in addition to the fact that ratings across the board are only weakly related, a possible explanation is that 
               manipulation of ratings may occur for many games. Especially for games published by smaller developers that aren’t widely
               publicized, which make up a majority of cases in this dataset, flooding a game’s page with positive reviews would be a 
               relatively simple task. This is an intriguing possibility, but given time constraints, it’s not something we deeply 
               investigated."),
               
               tags$p("Given the limitations of popularity metrics from third-party sites as well as Steam itself, we looked into the 
                      possibility of using Steam’s API to obtain popularity metrics for games directly from the source. However, Steam 
                      does not provide a direct way to access these metrics, possibly as a way to avoid speculation that might negatively 
                      impact promotion of certain video games. Third parties did not have reliable metrics either, because of changes 
                      Steam has made to its API over the years."),
               
               tags$hr(),
               
               ###
               tags$h3("Price vs. User Ratings"),
               tags$p("Diving in deeper to the data, we decided to investigate price vs user ratings across three different websites.
               We chose to look closely at the ratings from users on Steam (of course), IGDB, and Metacritic. Each of these websites 
               are highly active and have a large number of users. Our rationale was that different categories of games may inherently have
               higher development costs, or may be subject to different kinds of pressures from their users. Overall, we found a weak, and 
               positive, relationship between price and popularity across categories. In the below plot, we plot how much each extra percentage
               point in rating translates to change in price."),
               
               plotOutput("big_price_plotO"),
               
               tags$p("For sake of eliminating edge cases, we decided to ignore any games that are more than $70, as there are very few. After 
                      plotting and examining our results, we found that there is little correlation between user ratings and price of each 
                      video game. Ratings tend to be less variable when observing the results from the Steam user ratings, and there was less
                      overall correlation between price and user scores when looking at either Metacritic or IGDB. In conjunction with our 
                      previous results, this might be because of manipulation, since Steam is the actual platform where users buy games and
                      it would be more advantageous for developers to boost ratings on Steam."),
               
               tags$hr(),
               
               ###
               tags$h2("How the Steam Store Changed Over Time"),
               tags$p("Steam was released relatively recently, which gives us a limited amount of data that we can draw general conclusions 
                      from, which would be taken into account. It seems as though Steam has been releasing more and more games steadily from
                      the period of its release to around 2020, where the number of games published seems to take a huge leap. This could be 
                      due to a number of reasons, however a likely reason could have been due to the Pandemic, COVID 19. This is a likely 
                      reason due to the dip in games released since that year, however it is difficult to say much definitively due to the 
                      lack of number of years since 2020."),
               
               plotOutput('priceToYear'),
               
               tags$p("When we examine price, it seems as though the lack of data available is quite limiting. We are unable to predict 
               meaningfully whether or not the average price or the number of games published per year will increase or decrease, however if 
               we continue to analyze considering COVID 19, we can say that it may have been likely that games published would have continued 
               to decrease until stabilization, and average price would likely have continued to fluctuate."),
      
               tags$hr(),
               
               ### 
               tags$h2("Ethics"),
               tags$p("Our investigation deals with factors that influence the price and overall sucess of video games. This affects three 
                      main players: video game developers, video game players, and middlemen like Steam. The data we found is gathered from
                      Steam, which has an incentive to increase sales and profits of the video games it sells. The possibility that Steam isn't
                      fully transparent with the data it shares must be considered. At the same, this data was gathered by a third-party source, 
                      which might have different incentives. One of the main problems with our investigation is that there is no reliable data 
                      on video game popularity that we could find. This makes sense - video game developers and Steam want to make their video
                      games seem like quality products that stand apart from other games. However, this ignores the vested interest of video game
                      players, who currently don't have a objective standard with which they can evaluate the quality of games before they buy
                      them."),
               
               tags$hr(),
               
               ###
               tags$h2("Data Context"),
               tags$p("This data was pulled from Steam’s API in 2022. Ratings were scraped from a number of different video game rating
                      websites. It represents all games available on the Steam store as of that date, including game’s name, price, genre
                      categories, ratings from third-party websites, and more. The data excludes games that are free to play."),
               
               plotOutput('price_dense_intro'),
               
               tags$p("We wanted to study factors that influence the ratings and popularity of video games on Steam, one of which included 
                      price. The majority of prices are less than $10 U.S. dollars.")
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
>>>>>>> 5ba017a0024088eb2c790f9e98928f26078a95f7
