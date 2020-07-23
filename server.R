library(shiny)
library(dplyr)
library(ggplot2)


math <- read.table("student-mat.csv",sep= ";", header= T)

shinyServer(function(input, output, session) {
  
  # Create a new reactive variable
  newVar <- reactive({
    newDat <- math %>% filter(school == input$school)
  })
  
  
  # Create a scatter plot
  output$G12Plot <- renderPlot({
    newDat <- newVar()
    
    g <- ggplot(newDat, aes(x = G1, y = G2))
    
    if(input$sex){
      g + geom_point(size = input$size, aes(col = sex))
    } else {
      g + geom_point(size = input$size)
    }
  })
  
  # Create text info
  output$info <- renderText({
    newDat <- newVar()
    
    paste("The average first period grade for math", input$school, "is", 
          round(mean(newDat$G1, na.rm = TRUE), 2), 
          "and the average second period grade is", 
          round(mean(newDat$G2, na.rm = TRUE), 2), sep = " ")
  })
  
  # Create output of observations
  output$table <- renderTable({
    newVar()
  })
  
})
