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


# UI part
ui <- dashboardPage(skin = "blue",
                    # Add title                  
                    dashboardHeader(title = "Student Performance Data Analysis", titleWidth = 350),
                    
                    # Define sidebar items
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(tabName = "intro", "Introduction", icon = icon("archive")),
                        menuItem(tabName = "data", "Data Exploration", icon = icon("table")),
                        menuItem(tabName = "unsuper", "Unsupervised Learning", icon = icon("th")),
                        menuItem(tabName = "mlr", "Multiple Linear Regression", icon = icon("tablet")),
                        menuItem(tabName = "tree", "Ensemble Model", icon = icon("tablet")),
                        menuItem(tabName = "sub", "Data Information", icon = icon("th"))
                        )),
                    
                    # Define the body of the app
                    dashboardBody(
                      tabItems(
                        
################## "Introduction" tab ############################################################################
                        tabItem(tabName = "intro",
                                fluidRow(
                                  # Add in latex function 
                                  withMathJax(),
                                  
                                  # Two columns for each of the two items
                                  column(6,
                                         # Description of data
                                         h1("Data Description"),
                                         # Box to contain the data description content
                                         box(background = "navy", width = 12,
                                             h4("The dataset is from UCI Machine Learning Repository and it can be found from", 
                                                a("this website", 
                                                  href = "https://archive.ics.uci.edu/ml/datasets/Student+Performance"), "."),
                                             h4("It records the students' academic performance in math and Portuguese language
                                                from two Portuguese schools, 'GP' - Gabriel Pereira and 'MS' - Mousinho da Silveira.",
                                                "In this shiny app, we will focus on the records of students' math grades 
                                                from two schools and then discuss the record of language grade in the future. 
                                                There are 33 variables in total from the dataset above: 
                                                32 predictive attributes and 1 goal field, 
                                                which can be viewed in detail on the last page 'Data Page'. "),
                                             h4("Among these 32 predictors, it has found the first grade(G1) and the second grade(G2) 
                                                contribute more to the target, the final grade(G3) than other 30 variables and thus
                                                we specifically focus on these two grades in our final grade analysis.
                                                Detailed analysis pages are listed on the left navigation. ")
                                          
                                          )),
                                  
                                  column(6, 
                                         # Ability of the APP
                                         h1("Ability of the APP"),
                                         # Box to contain the ability of this app
                                         box(background = "blue", width = 12,
                                             h4("The control for this app is located on the left and the visualizations are given on the right. "),
                                             h4("In the 'Data Exploration' page, a scatter plot has been made to 
                                                visualize the relationship between the first and second grades for students from two school.
                                                You can see the different plots and values by choosing different schools and 
                                                save the scatter plot by clicking the download button at the bottom of this page"),
                                             h4("In the 'Unsupervised Learning' page, a PCA method is utilized to see 
                                                the linear combinations of those variables. 
                                                And you can see a biplot, a screeplot and 
                                                plots of Proportion of Variance Explained and Cum. Prop of Variance Explained respetively 
                                                by clicking different tabs."),
                                             h4("In the 'Multiple Linear Regression' page, you can see predict the final grade after inputting 
                                                the values of the first grade and the second grade. Details have beed discussed during this page"),
                                             h4("In the 'Ensemble Model', we have compared two different tree models - knn and random forest, 
                                                for some variables. You can choose to input variables you would like to explore in this page"), 
                                             h4("In the 'Data Information' page, you can scroll through the whole dataset and 
                                                also search for data you are interested in."),
                                             h4("The code for this shiny app can be access from my", 
                                                a("github page", 
                                                  href = "https://github.com/jiangyu1208/ST558-Summer20-Project-3"), ".")
                                             
                                             )))),
                        
######################################### "Data Exploration" tab ##############################################################
                        tabItem(tabName = "data",
                                fluidPage(
                                  
                                  # Application title
                                  titlePanel("Data Exploration for Two Schools"),
                                  
                                  # Sidebar with options for the data set
                                  sidebarLayout(
                                    sidebarPanel(
                                      h3("Select the Schools:"),
                                      selectizeInput("school", "School", selected = "GP", 
                                                     choices = levels(as.factor(mathdat$school))),
                                      br(),
                                      sliderInput("size", "Size of Points on Graph",
                                                  min = 1, max = 10, value = 5, step = 1),
                                      checkboxInput("sex", 
                                                    h4("Color Code Gender Status", style = "color:blue;"))
                                    ),
                                    
                                    # Show output
                                    mainPanel(
                                      plotOutput("tab2_plot1"),
                                      textOutput("tab2_text1"),
                                      tableOutput("tab2_table1"),
                                      downloadButton("download_tab2_plot1", "Save the Scatter Plot between G1 and G2")
                                    )
                                  )
                                )),
########################################## "Unsupervised Learning" tab ###############################
                        tabItem(tabName = "unsuper",
                                fluidPage(
                                  headerPanel(h1("Principal Component Analysis")),
                                  
                                  # Sidebar with options for the two schools
                                  sidebarLayout(
                                    sidebarPanel(
                                      h3("Select the Schools:"),
                                      selectizeInput("school", "School", selected = "GP",
                                                     choices = levels(as.factor(mathdat$school)))
                                    ),
                                    mainPanel(
                                      tabsetPanel(
                                        
                                        tabPanel("PC_Scores",verbatimTextOutput("scores")),
                                        tabPanel("PC_Scree_PLot",plotOutput("screeplot")),
                                        tabPanel("Bi_Plot",plotOutput("biplot")),
                                        tabPanel("Proportion of Variance Explained vs 
                               Cum. Proportion of Variance Explained",
                                                 plotOutput("explain"))
                                        
                                      )))
                                )),
######################################## "MLR" tab ##############################################
                      tabItem(tabName = "mlr",
                              pageWithSidebar(
                                # Application title
                                headerPanel("Linear Regression Analysis"),
                                
                                # Adding widgets
                                sidebarPanel(
                                  h4("Help text"),
                                  helpText("Enter the First Grade and the Second Grade ,
             click on ",strong("Predict!")," to get predicted value of Final grade."),
                                  numericInput("G1", label = "the First Grade",value = 10, min = 3, max = 19),
                                  helpText("The range of the first grade is from 3 to 19 points."),
                                  br(),
                                  
                                  sliderInput("G2", label = "the Second Grade", 
                                              value = 10, min = 0, max = 19, animate = FALSE),
                                  helpText("The range of the second grade is from 0 to 19 points."),
                                  
                                  
                                  actionButton("actionButton","Predict!",align = "center"),
                                  helpText("Note : for ",strong("Summary,Plot"), " and",
                                           strong("Table")," whole",strong("math"),
                                           "dataset is taken into account and results are displayed respectively.")
                                  
                                ),
                                
                                # Show a tabset that includes mpg prediction, plot, summary, and table view of mtcars dataset
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("Prediction",
                                             h2("Multiple Regression Predction ",align="center") ,
                                             p("A multiple linear regression (MLR) model that describes a ",
                                               strong("dependent variable y by independent variables x1, x2, ..., xp with error.")), 
                                             p("For example, the dataset from UCI Machine Learning Repository",
                                               strong("Studenet Performance")," 
               from records of the final grade for students from two Portuguese schools.",
                                               strong("lm")," is 
                 ", strong("used to fit linear model.")," 
                 Choose a",
                                               strong("model by AIC in a stepwise Algorithm"),
                                               "The obtained model has 2 (G1 and G2) independent variables.") ,
                                             h3("Predction Interval for the Final Grade"),
                                             
                                             p("We now apply the predict function and 
                 set the predictor variable in the newdata argument. 
                 We also set the interval type as",
                                               strong("predict")," and use",
                                               strong(" the default 0.95 confidence level")),
                                             
                                             p("The predicted",span("the Final Grade",style = "color:blue")," 
                 and its",span(" Lower Bound",style="color:blue"),"and",
                                               span("Upper Bound",style="color:blue")," values are :"),
                                             
                                             code(textOutput("final"))
                                    ),
                                    
                                    tabPanel("Summary", 
                                             h2("Summary of",strong("math"),"dataset"),
                                             verbatimTextOutput("summary")), 
                                    tabPanel("Table", 
                                             h2("Generate an HTML table view of math dataset"),
                                             tableOutput("table.2")),
                                    tabPanel("Diagnostic Plots" ,
                                             h2("Diagnostic plots provide checks for heteroscedasticity, 
                  normality, and influential observerations."),
                                             plotOutput("myplot"))
                                  )
                                )
                              )),

################################ "Ensemble Model" tab ##################################################
                      tabItem(tabName = "tree",
                              pageWithSidebar(
                                headerPanel("Ensemble Model"),
                                sidebarPanel(
                                  conditionalPanel(condition="input.conditionedPanels==1",
                                                   h3("Exploratory Plots"),
                                                   selectInput("var1", "Select variable for x axis", 
                                                               choices = c("G1", "G2", "G3"), 
                                                               selected = "G1"),
                                                   
                                                   selectInput("var2", "Select variable for y axis", 
                                                               choices = c("G1", "G2", "G3"), 
                                                               selected = "G2"),
                                                   
                                                   selectInput("var3", "Select a factor variable for color", 
                                                               choices = c("school", "sex", "paid"), 
                                                               selected = "school")
                                  ),
                                  conditionalPanel(condition="input.conditionedPanels==2",
                                                   h3("Compare between two models"),
                                                   selectInput("var4", "Select variable", 
                                                               choices = c("school", "sex", "paid"), 
                                                               selected = "school"),
                                                   
                                                   selectInput("var5", "Select predictor variables (minimum 2)", 
                                                               choices = c("G1", "G2", "G3", "school", "sex", "paid"), 
                                                               selected = c("G1", "G2"), multiple = TRUE),
                                                   checkboxInput("showModel1", "Show/Hide rpart model", 
                                                                 value = FALSE),
                                                   checkboxInput("showModel2", "Show/Hide Random Forest model", 
                                                                 value = FALSE)
                                  ) 
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("Panel 1", plotOutput("plot1"), value = 1), 
                                    tabPanel("Panel 2", plotOutput("plot2"), tableOutput("table.3"), value = 2)
                                    , id = "conditionedPanels"
                                  )
                                )
                              )),

##################################### "Data Page" tab #####################################################
                      tabItem(tabName = "sub",
                              mainPanel(
                                
                                downloadButton("downloadTable", "Download the Dataset of the Record of Math Grade")
                              ))
                      
                      
                    ),
))
