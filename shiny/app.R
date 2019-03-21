library(shiny)
#library(shinyjs)
library(shinyBS)
library(leaflet)
library(dplyr)
library(tidyr)
library(caret)

# read data
clusteredData <- read.csv("./www/clustered_data.csv", header = TRUE, sep=",")


# lod model: logRegModel
load("./www/logRegModel.Rdata");

payStatusName = c("Inactive"=-2,"Paid in Full"=-1,"Paid Minimum Due"=0,"Month Delay"=1, "Two-Month Delay"=2,
                  "Three-Month Delay"=3,"Four-Month Delay"=4,"Five-Month Delay"=5,"Six-Month Delay"=6,
                  "Seven-Month Delay"=7,  "Eight-Month Delay"=8,"Nine-Plus"=9)
edName = c("Graduate school"=1,"University"=2,"High school"=3,"Other"=4)
ageName = c("20+"=25,"30+"=35,"40+"=45,"50+"=55,"60+"=65)
limitInt = c(50000,100000,150000,250000,350000,500000)
limitIntName = c("<$50,000","<$100,000","<$150,000","<$250,000","<$350,000","<$500,000","$500,000+")
billInt = c(15000,30000,45000,60000,75000,90000,150000)
billIntName = c("<$15,000","<$30,000","<$45,000","<$60,000","<$75,000","<$90,000","<$150,000","$150,000+")
sexName = c("M"=1, "F"=2)
mStatus = c("Married"=1,"Single"=2,"Divorced"=3,"Other"=0)



# Define UI for application that draws a histogram
ui <- bootstrapPage( theme = "styles.css",
   div(class = "title", titlePanel(title = h2(class="heading", "Credit Card Default Research"))),
   div( class = "outer",
        absolutePanel( id = "controls", class = "control-panel", 

              selectInput('age', 'Age:', ageName, selected = ageName[2]),
              selectInput('sex', 'Sex:', sexName, selected = sexName[2]),
              selectInput('education', 'Education:', edName, selected = edName[2]),
              selectInput('marriage', 'Marital Status:', mStatus),
              selectInput('standing', 'Clinet Standing:', payStatusName, selected = payStatusName[3]),
              sliderInput("lastPayAmt", "Amount Paid Last Month (NT$):",
                         min = 0, max = 500000,
                         value = 10000, step = 50),
              sliderInput("lastBillAmt", "Last Month Bill Amount (NT$):",
                         min = 0, max = 500000,
                         value = 22000, step = 50),
              sliderInput("limit", "Credit Limit (NT$):",
                         min = 500, max = 1000000,
                         value = 90000, step = 500),
              actionButton("evaluate", "Evaluate the Client", class = "btn")
        ),
        absolutePanel( class = "prediction-panel", 
                         textOutput("prediction")
        )
                          
))

result = NULL

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  

  
  observeEvent(input$evaluate, {
    # get the data from the input fields
    
    data = data.frame(
        name = c("SEX", "AGE", "EDUCATION", "PAY_1","PAY_AMT1", "BILL_AMT1","LIMIT_BAL","MARRIAGE"),
        value =as.numeric(c(input$sex, input$age, input$education, input$standing ,input$lastPayAmt, 
                 input$lastBillAmt, input$limit,input$marriage  ))
    )
    
    data = spread(data, name, value)
    
    standing  = 
    
    # replicate the missing data (past bills , amount paidand standing)
    data = data %>% mutate(BILL_AMT2 = BILL_AMT1,BILL_AMT3 = BILL_AMT1,BILL_AMT4 = BILL_AMT1,
     BILL_AMT5 = BILL_AMT1,BILL_AMT6 = BILL_AMT1, PAY_AMT2 = PAY_AMT1, PAY_AMT2 = PAY_AMT1, 
     PAY_AMT3 = PAY_AMT1, PAY_AMT4 = PAY_AMT1, PAY_AMT5 = PAY_AMT1,  PAY_AMT6 = PAY_AMT1,
     PAY_2 = PAY_1,PAY_3 = PAY_1,PAY_4 = PAY_1,PAY_5 = PAY_1,PAY_6 = PAY_1); 
    
    default = round(predict(logRegModel, newdata =  data, type = "prob")[,1]*100, digits = 2)
    
    
    r = data
    showNotification(paste0(default,"%")
                     )
    #data <- spread(inputData(), name, value)
    #data$WindDir3pm = as.factor(data$WindDir3pm)
    #p <- paste0(round(predict(logRegModel, newdata =  data, type = "prob")[,"1"]*100, digits = 2),"%")
    
 #   clara = clara(numVicitms, k = as.numeric(input$k), metric = as.character(input$m),stand = F, samples = 80, sampsize =200)
#    clusters <<- clara[["clustering"]]
    #  result <<- input$age
   
#    showNotification("Message text",
 #                    action = a(href = "javascript:;", "Reload page")
  #  )     
  })

#  randomVals <- eventReactive(input$evaluate, {
 #   session$sendCustomMessage(type = 'testmessage',
                         #     message = 'Thank you for clicking')
#  })  

#  output$prediction <- renderText({
#    result
#  })
  
 # output$evaluate <- reactive({
#    data <- spread(inputData(), name, value)
#    data$WindDir3pm = as.factor(data$WindDir3pm)
#    p <- paste0(round(predict(logRegModel, newdata =  data, type = "prob")[,"1"]*100, digits = 2),"%")
#  })
  
  
#  inputData <- reactive({
#    data.frame(
#      name = c("age", "sex",  "education", "standing", "lastPaymentAmt", "nextBillAmt","limit"),
#      value =c(input$SEX, input$AGE_GROUP, input$EDUCATION, input$Lastpayment, input$Nextpaymentamount, input$LIMIT_BAL )
#    )
#  })
  
#  output$prediction <- reactive({
#    data <- spread(inputData(), name, value)
   
#    p <- paste0(round(predict(logRegModel, newdata =  data, type = "prob")[,"1"]*100, digits = 2),"%")
#  })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

