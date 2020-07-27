# Load all the libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(data.table)
library(caret)
library(rpart)
library(gridExtra)
library(lattice)
library(e1071)
library(randomForest)

# Read the data
temp <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00356/student.zip",
              temp, mode="wb")
unzip(temp, "student-mat.csv")
mathdat <- read.table("student-mat.csv",sep= ";", header= T)
unlink(temp)
#(math <- as_tibble(math))

# Fit MLR
model <- lm(G3 ~ G1 + G2, data = mathdat )

# Choose a model by AIC in a Stepwise Algorithm
best_model <- step(model, direction="both")

# generate model summary
summary(best_model)

# Shiny Part
shinyServer(function(input, output, session) {

##################################### Data Exploration ######################################################    
  # Create a new reactive variable
  newVar <- reactive({
    newDat <- mathdat %>% filter(school == input$school)
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
  
########################################## PCA ###############################################
  math <- reactive({
    subset(mathdat, school == input$school, select = c(G1, G2, G3, absences, studytime))
  })
  
  
  output$scores <- renderPrint({
    X <- math()
    pca <- princomp(X)
    pca$scores
  })
  
  output$screeplot <- renderPlot(
    {
      data <- math()
      dat <- princomp(data)
      screeplot(dat, type = "lines")
    })
  
  output$biplot <- renderPlot(
    {
      data <- math()
      dat <- princomp(data)
      biplot(dat, xlabs = rep(".", nrow(data)), cex = 1.2)
    })
  
  output$explain <- renderPlot(
    {
      data <- math()
      dat <- princomp(data)
      par(mfrow = c(1, 2))
      plot(dat$sdev^2/sum(dat$sdev^2), xlab = "Principal Component", 
           ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = 'b')
      plot(cumsum(dat$sdev^2/sum(dat$sdev^2)), xlab = "Principal Component", 
           ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')
    })
  
################################################# MLR #####################################################  
  # Reactive expression to predict the final. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from  this expression
  
  output$final <- renderText({ 
    input$actionButton
    isolate({
      # G1 , G2, sex
      newdata = data.frame(G1 = G1(), G2 = input$G2)
      final  <- predict(best_model, newdata , interval = "predict")
    })
  })
  
  
  # Generate diagnostic plot s
  output$myplot <- renderPlot({
    
    # optional 4 graphs/page
    layout(matrix(c(1,2,3,4), 2, 2, byrow=T))
    plot(best_model)
    
  })
  
  
  G1 <- reactive({
    first <- as.numeric(input$G1)
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(mathdat)
  })
  
  # Generate an HTML table view of the data
  output$table.2 <- renderTable({
    data.frame(mathdat)
  })
  
  
})
