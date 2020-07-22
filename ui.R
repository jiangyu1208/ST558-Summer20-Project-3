library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "blue",
  # Add title                  
  dashboardHeader(
    title = "Student Performance Data Analysis",
    titleWidth = 350),
  
  # Define sidebar items
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "intro", "Introduction", icon = icon("dashboard")),
      menuItem(tabName = "data", "Data Exploration", icon = icon("table")),
      menuItem(tabName = "unsuper", "Unsupervised Learning", icon = icon("th")),
      menuItem(tabName = "model", "Modeling", icon = icon("tablet"))
    )),
  
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
            # Box to contain the content
            box(background = "blue", width = 12,
              h4("This data is from UCI Machine Learning Repository")
              
              
              
            )
            
            
          ),
          
          column(6, 
                 # Ability of the APP
                 h1("Ability of the APP"))
              )
    ),
    
    # "Data Exploration" tab
    tabItem(tabName = "data"),
    
    # "Unsupervised Learning" tab
    tabItem(tabName = "unsuper"),
    
    # "Modeling" tab
    tabItem(tabName = "model")
    
  )
 ))
