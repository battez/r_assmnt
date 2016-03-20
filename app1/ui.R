library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
  
  # Application title
  titlePanel("Programming for Data - R assignment"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(position = "right",
    sidebarPanel(
      
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("rock", "pressure", "cars")),
      sliderInput("bins",
                  "Bob:",
                  min = 2,
                  max = 30,
                  value = 30),
      img(src='http://g-ecx.images-amazon.com/images/G/01/img15/pet-products/small-tiles/23695_pets_vertical_store_dogs_small_tile_8._CB312176604_.jpg')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("distplot")), 
                  tabPanel("Summary", verbatimTextOutput("summary")), 
                  tabPanel("Table", tableOutput("table"))
                  
      
    )
  )
  )
))