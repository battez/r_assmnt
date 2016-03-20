library(shiny)
library(datasets)
aq <-  read.table("../airquality.csv", header=TRUE, 
                       sep=",")
df <- aq[complete.cases(aq), ]

df$Full <- factor(df$Month, levels = 1:12, labels = month.name)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  

  
  output$mainPlot <- renderPlot({
   
    x <- input$scatter
    plot(df[, x], df$Ozone, main = paste("Plot: Ozone and", x), ylab = 'Ozone', xlab = x)
    
    
   if(input$toggle == TRUE)
   {
     model <- lm(df$Ozone ~ df[ ,x])
     abline(model, lwd = 1, col="blue")
   }

    
    
  })
  
  output$distPlot <- renderPlot({
     
     x    <-   df[, input$column]
     bins <- seq(min(x), max(x), length.out = input$bins + 1)

   # draw the histogram with the specified number of bins
   hist(x, breaks = bins, col = 'red', main=paste('Histogram of', input$column))
   
  })
  
  output$summary <- renderPrint({
    print(paste('Summary statistics displayed for: ', input$column));
    summary(df[, input$column]);
    
    
  })
  
  output$table <- renderTable({
    # data.frame(x=datasetInput())
  })
})