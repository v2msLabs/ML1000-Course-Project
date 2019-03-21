library(shiny)
library(shinyBS)
library(leaflet)
library(dplyr)
library(tidyr)
library(caret)

# read data
defaultData <- read.csv("./www/clustered_data.csv", header = TRUE, sep=",")


# lod model: logRegModel
load("./www/logRegModel.Rdata");




# Define UI for application that draws a histogram
ui <- bootstrapPage( theme = "styles.css",
                     div(class = "title", titlePanel(title = h2(class="heading", "Credit Card Default Research"))),
                     div( class = "outer",
                          
                          
                          
                          absolutePanel( id = "controls", class = "control-panel", 
                                         
                                         #h3("Credit Card Default Reasearch"),
                                         
                                         #h4("Predict credit card  default"),
                                         
                                         #1 Input: Age value ----
                                         selectInput('AGE_GROUP', 'Age:', c("20+", "30+","40+","50+","60+")),
                                         
                                         selectInput('SEX', 'Sex:', c("M", "F")),
                  
                                         
                                         #2 Input: Education ----
                                         selectInput('EDUCATION', 'Education:', c("High School", "Graduate School","University","Unknown")),
                                         
                                        
                                        
                                         
                                         
                                         #3 Input: Last Payment  ----
                                         sliderInput("Lastpayment", "Last Payment:",
                                                     min = 0, max = 500000,
                                                     value = 10, step = 1),
                                         
                                         
                                         
                                         #4 Input: Next Payment Amount ----
                                         sliderInput("Nextpaymentamount", "Next Payment Amount:",
                                                     min = 0, max = 500000,
                                                     value = 10, step = 1),
                                         sliderInput("LIMIT_BAL", "LIMIT_BALANCE:",
                                                     min = 0, max = 500000,
                                                     value = 10, step = 1),
                                       
                                         submitButton("Evaluate!") 
                          ),
                          
                         
                          
                          absolutePanel( class = "prediction-panel", 
                                         # Output: prediction
                                        # h4("Chance of Rain Tomorrow"),
                                         textOutput("prediction")
                          )
                          
                     ))



# Define server logic required to draw a histogram
server <- function(input, output) {
  inputData <- reactive({
    data.frame(
      name = c("SEX", "AGE_GROUP",  "EDUCATION", "Lastpayment", "Nextpaymentamount", "LIMIT_BAL"),
      value =c(input$SEX, input$AGE_GROUP, input$EDUCATION, input$Lastpayment, input$Nextpaymentamount, input$LIMIT_BAL )
    )
  })
  
  output$prediction <- reactive({
    data <- spread(inputData(), name, value)
   
    p <- paste0(round(predict(logRegModel, newdata =  data, type = "prob")[,"1"]*100, digits = 2),"%")
  })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

