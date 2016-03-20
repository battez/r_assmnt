library(shiny)
library(datasets)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$distPlot <- renderPlot({
     x    <- faithful[, 2]  # Old Faithful Geyser data
     bins <- seq(min(x), max(x), length.out = input$bins + 1)

   # draw the histogram with the specified number of bins
   hist(x, breaks = bins, col = 'red', border = 'white')
  })
  
  output$summary <- renderPrint({
    summary(datasetInput())
  })
  
  output$table <- renderTable({
    data.frame(x=datasetInput())
  })
})