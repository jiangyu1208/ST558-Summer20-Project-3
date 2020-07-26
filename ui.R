library(shiny)
library(shinydashboard)
library(ggplot2)

# Read the data
temp <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00356/student.zip",temp, mode="wb")
unzip(temp, "student-mat.csv")
math <- read.table("student-mat.csv",sep= ";", header= T)
unlink(temp)
(math <- as_tibble(math))

ui <- dashboardPage(skin = "blue",
  # Add title                  
  dashboardHeader(title = "Student Performance Data Analysis", titleWidth = 350),
  
  # Define sidebar items
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "intro", "Introduction", icon = icon("archive")),
      menuItem(tabName = "data", "Data Exploration", icon = icon("table")),
      menuItem(tabName = "unsuper", "Unsupervised Learning", icon = icon("th")),
      menuItem(tabName = "model", "Modeling", icon = icon("tablet")))),
  
  # Define the body of the app
  dashboardBody(
    tabItems(
      
      # "Introduction" tab
      tabItem(tabName = "intro",
        fluidRow(
          # Add in latex function 
          withMathJax(),
          
          # Two columns for each of the two items
          column(6,
            # Description of data
            h1("Data Description"),
            # Box to contain the data description content
            box(background = "blue", width = 12,
              h4("This data is from UCI Machine Learning Repository")
              
    
            )),
          
          column(6, 
                 # Ability of the APP
                 h1("Ability of the APP"),
                 # Box to contain the ability of this app
                 box(background = "blue", width = 12,
                     h4("The control for this app is located on the left 
                        and the visualizations are given on the right. "))))),
    
    # "Data Exploration" tab
    tabItem(tabName = "data",
            fluidPage(
              
              # Application title
              titlePanel("Data Exploration for Two Schools"),
              
              # Sidebar with options for the data set
              sidebarLayout(
                sidebarPanel(
                  h3("Select the Schools:"),
                  selectizeInput("school", "School", selected = "GP", 
                                 choices = levels(as.factor(math$school))),
                  br(),
                  sliderInput("size", "Size of Points on Graph",
                              min = 1, max = 10, value = 5, step = 1),
                  checkboxInput("sex", 
                                h4("Color Code Gender Status", style = "color:blue;"))
                ),
                
                # Show output
                mainPanel(
                  plotOutput("G12Plot"),
                  textOutput("info"),
                  tableOutput("table")
                )
              )
            ))
    ),
    
    # "Unsupervised Learning" tab
    tabItem(tabName = "unsuper"),
    
    # "Modeling" tab
    tabItem(tabName = "model")
    
  )
 )
