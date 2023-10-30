library(tidyverse)
library(ggplot2)
library(shiny)
library(stringr)

songs <- read.csv("Spotify_top50_2021.csv")
songs.1 <- select(songs, danceability, energy, speechiness, acousticness, liveness)
songs.40 <- songs.1[1:40,]
songs.30 <- songs.1[1:30,]
songs.20 <- songs.1[1:20,]
songs.10 <-  songs.1[1:10,]
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Analyzing Spotify's Top 50 Songs of 2021"),
  sidebarLayout(
    sidebarPanel(
  # Select Box to chose variable being assessed 
  selectInput("select_var", h3("Select variable to assess"),
              choices= names(songs.1), selected = NULL),
      
    
      
  radioButtons("select_color", h3("Select color of histogram"),
               choices = list("orange" = 'orange', "purple" = 'purple',
                              "pink" = 'pink', "green"='green', "blue"='blue'), selected = 'purple'),
  radioButtons("num_observations", 
               h3("Select which group to observe:"), 
               choices = list("Top 50"= 1, "Top 40"= 2, "Top 30"=3, "Top 20"= 4, "Top 10"=5),
               selected = 1),
  # slider input for number of bins 
  sliderInput("bins",
              h3("Number of bins in histogram:"),
              min = 1,
              max = 50,
              value = 30),
  #option to show mean and sd
  checkboxInput("checkboxmean", label="Display mean", value=FALSE),
  
  checkboxInput("checkboxsd", label="Display standard deviation", value = FALSE),
  ),
  
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      hr(),
      
      fluidRow(column(5, verbatimTextOutput("mean"))),
    
      fluidRow(column(5, verbatimTextOutput("sd"))),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    data <- songs
    if(input$num_observations == 1){
      data<- songs.1
    }
    else if(input$num_observations == 2){
      data <- songs.40
    }
    else if(input$num_observations == 3){
    data <- songs.30
    }
    else if(input$num_observations == 4){
      data<- songs.20
    }
    else{
      data<- songs.10
    }
    
  title <- 'Distribution of Chosen Variable'
  if(input$select_var == "danceability"){
    title <- 'Distribution of Danceability Levels of Songs'
  }
  else if(input$select_var == "energy"){
    title <- 'Distribution of Energy Levels of Songs'
  }
  else if(input$select_var == "speechiness"){
    title <- 'Distribution of Speechiness Levels of Songs'
  }
  else if(input$select_var == "acousticness"){
    title <- 'Distribution of Acousticness Levels of Songs'
  }
  else{
    title <- 'Distribution of Liveness Levels of Songs'
  }
  
  x_cap <- "Chosen Variable"
  if(input$select_var == "danceability"){
    x_cap <- 'Danceability (0 to 1 scale)'
  }
  else if(input$select_var == "energy"){
    x_cap <- 'Energy (0 to 1 scale)'
  }
  else if(input$select_var == "speechiness"){
    x_cap <- 'Speechiness (0 to 1 scale)'
  }
  else if(input$select_var == "acousticness"){
    x_cap <- 'Acousticness (0 to 1 scale)'
  }
  else{
    x_cap <- 'Liveness (0 to 1 scale)'
  }
    # draw the histogram based on input$bins from ui.R
    hist(data[[input$select_var]], breaks = input$bins, main= title ,xlab=x_cap,col = input$select_color, border = 'darkgrey')
  })
  
  #Display mean if selected
  
  output$mean <- renderText({ 
    if(input$checkboxmean == TRUE & input$num_observations==1){
      
      paste("Mean:",mean(songs.1[[input$select_var]]))
    }
    else if(input$checkboxmean == TRUE & input$num_observations==2){
      paste("Mean:",mean(songs.40[[input$select_var]]))
    }
    else if(input$checkboxmean == TRUE & input$num_observations==3){
      paste("Mean:",mean(songs.30[[input$select_var]]))
    }
    else if(input$checkboxmean == TRUE & input$num_observations==4){
      paste("Mean:",mean(songs.20[[input$select_var]]))
    }
    else if(input$checkboxmean == TRUE & input$num_observations==5){
      paste("Mean:",mean(songs.10[[input$select_var]]))
    }
  })
  
  output$sd <- renderText({
    if(input$checkboxsd == TRUE & input$num_observations==1){
      
      paste("Standard Deviation:", sd(songs.1[[input$select_var]]))
    }
    else if(input$checkboxsd == TRUE & input$num_observations==2){
      paste("Standard Deviation:", sd(songs.40[[input$select_var]]))
    }
    else if(input$checkboxsd == TRUE & input$num_observations==3){
      paste("Standard Deviation:", sd(songs.30[[input$select_var]]))
    }
    else if(input$checkboxsd == TRUE & input$num_observations==4){
      paste("Standard Deviation:", sd(songs.20[[input$select_var]]))
    }
    else if(input$checkboxsd == TRUE & input$num_observations==5){
      paste("Standard Deviation:", sd(songs.10[[input$select_var]]))
    }
  })
  
  output$var <- renderPrint({as.numeric(input$select_var)})
}

# Run the application 
shinyApp(ui = ui, server = server)
