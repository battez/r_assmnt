library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
  # my css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  # Application title
  headerPanel("R Programming - Air Quality data in Shiny"),
  titlePanel("by Luke Barker, MSc. Data Engineering 2015/16."),
 
 
  # Sidebar with a slider input for the number of bins
  sidebarLayout(position = "right",
    sidebarPanel(
      width=5,
      radioButtons("scatter", "Plot - relationship between Ozone and:",
                   list("Solar.R","Wind", "Temp"),
                   selected ="Wind"),
      checkboxInput("toggle", 
                    label = "Toggle regression fit ", value = FALSE),
      hr(),
      selectInput("column", 
                  label = "Show Summary / Histogram for:",
                  choices = list("Ozone", "Solar.R","Wind", "Temp"),
                  selected = "Ozone"),
      
      sliderInput("bins",
                  "Pick no. of histogram bins:",
                  min = 2,
                  max = 25,
                  value = 15),
      
      
      img(src='http://capita.wustl.edu/capita/capitareports/GlobeO3/goodbad1.gif', width="100%")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      width=7,
      tabsetPanel(type = "tabs", 
          tabPanel("Plot", plotOutput("mainPlot")), 
          tabPanel("Summary Stats", verbatimTextOutput("summary")), 
          tabPanel("Histogram", plotOutput("distPlot"))
                  
      
      )
  )
  )
))