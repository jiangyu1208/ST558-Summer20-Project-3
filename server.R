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
  math.new <- reactive({
    subset(mathdat, school == input$school, select = c(G1, G2, G3, absences, studytime))
  })
  
  
  output$scores <- renderPrint({
    X <- math.new()
    pca <- princomp(X)
    pca$scores
  })
  
  output$screeplot <- renderPlot(
    {
      data <- math.new()
      dat <- princomp(data)
      screeplot(dat, type = "lines")
    })
  
  output$biplot <- renderPlot(
    {
      data <- math.new()
      dat <- princomp(data)
      biplot(dat, xlabs = rep(".", nrow(data)), cex = 1.2)
    })
  
  output$explain <- renderPlot(
    {
      data <- math.new()
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
  
################################################ Ensemble models ######################################### 
  ## convert sona cars1 data variables to factor
  
  math <- mathdat
  math$sex <- as.factor(math$sex)
  math$school <- as.factor(math$school)
  math$paid <- as.factor(math$paid)
  
  
  ## Input data from the first panel by reactive function
  
  selecteddata <- reactive({
    math[, c(input$var1, input$var2, input$var3)]
  }) 
  
  ## Output function for the plot in first panel
  
  output$plot1 <- renderPlot({
    df1 <- selecteddata()
    p <- ggplot(df1, aes(x = df1[,1], y = df1[,2], color = df1[,3]))
    p <- p + geom_point(size = 5) + geom_smooth(method = "lm")
    p <- p + labs(x = names(df1)[1], y = names(df1)[2], color = names(df1)[3])
    p
  })  
  
  ## Input data from the second panel by reactive function
  ## divide the data frame into test and train set
  
  cardata <- reactive({
    df2 <- math[, c(input$var4, input$var5)]
    set.seed(3)
    inTrain <- createDataPartition(y = df2[,1], p = 0.75, list = FALSE)
    traincar <- df2[inTrain,]
    testcar <- df2[-inTrain,]
    list(traincar = traincar, testcar = testcar)
  })
  
  ## Reactive function to fit prediction model
  
  preddata <- reactive({
    traincar <- cardata()$traincar
    testcar <- cardata()$testcar
    pred1 <- 0
    pred2 <- 0
    
    ## rpart 
    
    if(input$showModel1) {
      formula <- as.formula(paste(input$var4, '~ .' ))
      set.seed(2376)
      modelfit <- rpart(formula, method = "class", data = traincar)
      pred1 <- predict(modelfit, testcar, type = "class")
    }
    
    ## random Forest
    
    if(input$showModel2) {
      x <- traincar[,-1]
      y <- traincar[,1]
      set.seed(2376)
      modelfit2 <- randomForest(x, y, importance = TRUE, proximity = TRUE, ntree = 100)
      pred2 <- predict(modelfit2, testcar)
    }
    
    list(pred1 = pred1, pred2 = pred2)
  })
  
  ## Function to render plot
  
  output$plot2 <- renderPlot({
    
    traincar <- cardata()$traincar
    testcar <- cardata()$testcar
    p1 <- ggplot()
    p2 <- ggplot()
    
    if(input$showModel1) {
      pred1 <- preddata()$pred1
      p1 <- ggplot(testcar, aes(x = testcar[,1], y = pred1))
      p1 <- p1 + geom_point(size = 5, color = "red", alpha = 0.1)
      p1 <- p1 + labs(x = names(testcar)[1], y = "Prediction",
                      title = "Model 1/rpart")
    }
    
    
    if(input$showModel2) {
      pred2 <- preddata()$pred2
      p2 <- ggplot(testcar, aes(x = testcar[,1], y = pred2))
      p2 <- p2 + geom_point(size = 5, color = "blue", alpha = 0.1)
      p2 <- p2 + labs(x = names(testcar)[1], y = "Prediction",
                      title = "Model 2/Random Forest")
    }
    
    return(grid.arrange(p1, p2, ncol = 2))
    
  })
  
  ## Table to print confusion matrix
  
  output$table.3 <- renderTable({
    
    traincar <- cardata()$traincar
    testcar <- cardata()$testcar
    
    df3 <- data.frame(rpart = sample(0, 7, replace = TRUE), 
                      values = sample(0, 7, replace = TRUE))
    
    df4 <- data.frame(RandomForest = sample(0, 7, replace = TRUE), 
                      values = sample(0, 7, replace = TRUE))
    
    if(input$showModel1) {
      pred1 <- preddata()$pred1
      matrix1 <- confusionMatrix(pred1, testcar[,1])
      df3 <- data.frame(values = matrix1$overall)
      df3 <- setDT(df3, keep.rownames = TRUE)[]
      names(df3)[1] <- "rpart"
    }
    
    if(input$showModel2) {
      pred2 <- preddata()$pred2
      matrix2 <- confusionMatrix(pred2, testcar[,1])
      df4 <- data.frame(values = matrix2$overall)
      df4 <- setDT(df4, keep.rownames = TRUE)[]
      names(df4)[1] <- "RandomForest"
    }
    
    cbind(df3, df4)
  })

})
