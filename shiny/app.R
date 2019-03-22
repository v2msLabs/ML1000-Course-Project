library(shiny)
library(shinyjs)
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
ageInt = c(20,30,40,50,60)
ageName = c("20+"=25,"30+"=35,"40+"=45,"50+"=55,"60+"=65)
limitInt = c(50000,100000,150000,250000,350000,500000)
limitIntName = c("<$50,000","<$100,000","<$150,000","<$250,000","<$350,000","<$500,000","$500,000+")
billInt = c(15000,30000,45000,60000,75000,90000,150000)
billIntName = c("<$15,000","<$30,000","<$45,000","<$60,000","<$75,000","<$90,000","<$150,000","$150,000+")
sexName = c("M"=1, "F"=2)
mStatus = c("Married"=1,"Single"=2,"Divorced"=3,"Other"=0)

cluster = 1;
# Define UI for application that draws a histogram
ui <- bootstrapPage( theme = "styles.css",
                     useShinyjs(), 
 #  div(class = "title", titlePanel(title = tags$div(class="heading", "Credit Card Default Research"))),
   div( class = "outer",
        div(id="bkg"),
        absolutePanel( id = "controls", class = "control-panel", 

              selectInput('age', 'Age:', ageName, selected = ageName[2]),
              selectInput('sex', 'Sex:', sexName, selected = sexName[2]),
              selectInput('education', 'Education:', edName, selected = edName[2]),
              selectInput('marriage', 'Marital Status:', mStatus),
              selectInput('standing', 'Clinet Standing:', payStatusName, selected = payStatusName[3]),
              sliderInput("lastPayAmt", "Amount Paid Last Month (NT$):",
                         min = 0, max = 100000,
                         value = 1000, step = 50),
              sliderInput("lastBillAmt", "Last Month Bill Amount (NT$):",
                         min = 0, max = 200000,
                         value = 22000, step = 50),
              sliderInput("limit", "Credit Limit (NT$):",
                         min = 500, max = 500000,
                         value = 90000, step = 100),
              actionButton("evaluate", "Evaluate the Client", class = "btn")
        ),
        absolutePanel(id = "cluster-1", class="prediction-panel, hide",

          tags$img(src="folks.jpg"),
          div(class="tile-img",
              tags$p("Cluster #1 - The Regular Folks. Nothing particular stands out about this group. Majority of the clients in this 
                     group pay required minimum. small percentage of the group delay the payments but 
                     no more than 2 months. Their monthly bills rarely go beyond 75,000 Taiwanese dollars. 
                     Some members of this cluster have high credit limit, but it does not look like they 
                     take advantage of it. They are rather well educated, range between 20 and 40 years of 
                     age. This is the largest category that describes pretty accurately the main mass of 
                     the cardholder - 43.7%")
          )
        ),
        absolutePanel(id = "cluster-2", class="prediction-panel, hide",
                    tags$img(src="spenders.jpg"),
                    div(class="tile-img",
                        tags$p("Cluster #2 - The Exuberant Spenders. People in this cluster live the life! Their monthly bills often go beyond 
                               $150,000 mark. Many group members have a credit limit in the range 250,000 
                               or more. As the *Regular Folks* the cardholder of this class do not pay the 
                               full bill amount. There is a good sector of the clients that are 3 month 
                               behind with the payment. Education-wise the group almost equally split 
                               between the university and graduate school graduates. Just like in the case 
                               of the first cluster majority of the people in this group are between 20 and 
                               40 years old, where 30+ age group dominates. This is the third largest 
                               group of the credit card holder population - 14.8%")
                        )
                        ),
        absolutePanel(id = "cluster-3", class="prediction-panel, hide",
                    tags$img(src="realists.jpg"),
                    div(class="tile-img",
                        tags$p("Cluster #3 - The Realists. The second largest group of the clients (32.7%) are people who really 
                               keep their spending in check. The vast majority of the people in this 
                               cluster have the bill amount less than $15,000 NT (about 650 CAD). 
                               Unlike the previous two groups almost half of the realists pay their 
                               bills in full. Only small percentage have delayed the payment by a month. 
                               The realists are generally older. Half of the group finished the graduate 
                               school, the second largest education group are university graduates. 
                               The credit limit of the group varies and could be very high but they do not 
                               fall victims to seduction!")
                        )
                        ),
        absolutePanel(id = "cluster-4", class="prediction-panel, hide",
                    tags$img(src="grinders.jpg"),
                    div(class="tile-img",
                        tags$p("Cluster #4 - The Grinders. The striking difference between this cluster of people and the others is 
                               a default rate; it is about 65%! The majority of the grinders have relatively low bills: 
                               $45,000 or less. Smaller credit limits, - less than 100,000. Yet, predominantly they are 
                               two or more month late with their payments. This is a highly educated group, majority of which 
                               are university graduates. Wait a minute... Age-wise this is the youngest group of all, 
                               where over 40% a people in their twenties. Maybe they are still students? Thankfully this is 
                               the smallest group that make 8.6 of the total population of the cardholders")
                        )
                        ),      
        absolutePanel(id = "default", class="default, invisible",
                      div( class = "default-text",textOutput("prediction")))
))

default = 0


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  rv <- reactiveValues()

  observeEvent(input$evaluate, {
    # get the data from the input fields
    data = data.frame(
        name = c("SEX", "AGE", "EDUCATION", "PAY_1","PAY_AMT1", "BILL_AMT1","LIMIT_BAL","MARRIAGE"),
        value =as.numeric(c(input$sex, input$age, input$education, input$standing ,input$lastPayAmt, 
                 input$lastBillAmt, input$limit,input$marriage  ))
    )

    data = spread(data, name, value)

    # replicate the missing data (past bills , amount paid and standing)
    data = data %>% mutate(BILL_AMT2 = BILL_AMT1,BILL_AMT3 = BILL_AMT1,BILL_AMT4 = BILL_AMT1,
     BILL_AMT5 = BILL_AMT1,BILL_AMT6 = BILL_AMT1, PAY_AMT2 = PAY_AMT1, PAY_AMT2 = PAY_AMT1, 
     PAY_AMT3 = PAY_AMT1, PAY_AMT4 = PAY_AMT1, PAY_AMT5 = PAY_AMT1,  PAY_AMT6 = PAY_AMT1,
     PAY_2 = PAY_1,PAY_3 = PAY_1,PAY_4 = PAY_1,PAY_5 = PAY_1,PAY_6 = PAY_1); 
    # predict default
    p = predict(logRegModel, newdata =  data, type = "prob")

    rv$default <- paste0("Chance of Default: ", round(p[1,"1"]*100, digits = 0),"%")
    client = data[1,];
    # find the cluster
    clusters = clusteredData %>% filter(EDUCATION == client$EDUCATION & 
          PAY_1 == client$PAY_1 &
          SEX == client$SEX &
          MARRIAGE == client$MARRIAGE &
          AGE_GROUP == names(ageName[findInterval(client$AGE,ageName)]) &
          BILL_AMOUNT == billIntName[1+findInterval(client$BILL_AMT1,billInt)] &
          CREDIT_LIMIT == limitIntName[1+findInterval(client$LIMIT_BAL,limitInt)]  
    )
    n = nrow(clusters)
    newCluster = 0
    if (n  > 1L) { 
      # pick the secpnd most populous. Cluster one will always be present due to the fact we mock the bill & payment history
      tmp = clusters %>% group_by(CLUSTER) %>% summarize(count = n()) %>% arrange(desc(count))
      newCluster =  tmp[2,]$CLUSTER
      
    } else if (n == 1L) {
      newCluster = clusters[1,]$CLUSTER
    } else {
      newCluster = 1
    }
    if (is.na(newCluster)) {
      newCluster = 1
    }
    newId = paste0("#cluster-",newCluster)
    id = paste0("#cluster-",cluster)
    #hide all
    shinyjs::addClass(class ="hide", selector = id)
    # show new
    shinyjs::removeClass(class ="hide", selector = newId)
    shinyjs::addClass(class ="prediction-panel", selector = newId)
    shinyjs::removeClass(class ="invisible", selector = "#default")
    shinyjs::addClass(class ="default", selector = "#default")
    # update global
    cluster <<- newCluster
  })


  output$prediction <- renderText({
    rv$default
  })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

