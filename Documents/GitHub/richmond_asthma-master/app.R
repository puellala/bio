library(readxl)
library(shiny)
library(shinydashboard)
library(dplyr)

decisionTree <- read_xlsx("Decision_Tree_Unmerged.xlsx")
colnames(decisionTree)[1] <- "Age"
colnames(decisionTree)[2] <- "School"
colnames(decisionTree)[3] <- "Asthma"
colnames(decisionTree)[4] <- "Care"


ui <- fluidPage(
  titlePanel("Richmond Childhood Asthma-Referral Pathway"),
  sidebarLayout(
    sidebarPanel(
      helpText("Please select options from the dropdown boxes for each condition."),
       htmlOutput("age_selector"),
       htmlOutput("school_selector"),

       selectInput("asthma",
                   label = "Condition: Asthma-Related ED Visit or Inpatient Admission Within Past 12 Months",
                   choices = decisionTree$Asthma),
       selectInput("care",
                   label = "Condition: Receiving Care at VCUHS",
                   choices = decisionTree$Care)
    ),

    mainPanel(br(), br(), br(), HTML(paste0("<b>","RECOMMENDED ACTIONS:","</b>")), br(), br(),br(),
              fluidRow(
                infoBox(icon = icon("ambulance", "fa-3x"),
                        HTML(paste0("<b>","Mandatory Actions:","</b>")), br() 
                ),
                infoBox( icon = icon("medkit", "fa-3x"), 
                         HTML(paste0("<b>","Optional:","</b>")), br() 
                ),
                infoBox( icon = icon("exclamation-triangle", "fa-3x"),
                         HTML(paste0("<b>","Do Not:","</b>")), br() 
                )
              ),
              
              fluidRow(
                box(
                  width = 4,
                  uiOutput("mandatory1"), 
                  uiOutput("mandatory2"),
                  uiOutput("mandatory3"),
                  uiOutput("mandatory4")
                ),
                box(
                  width = 4,
                  uiOutput("optional1"),
                  uiOutput("optional2"),
                  uiOutput("optional3"),
                  uiOutput("optional4")
                ),
                box(
                  width = 4,
                  uiOutput("doNot1"),
                  uiOutput("doNot2"),
                  uiOutput("doNot3"),
                  uiOutput("doNot4")
                )
              )
    )
  )
)

server <- function(input, output) {
  output$age_selector = renderUI({
    
    selectInput("age",
              label = "Condition: Age",
              choices = decisionTree$Age)
  })
  output$school_selector = renderUI({
    school_available = decisionTree[decisionTree$Age == input$age, "School"]
    selectInput("school",
              label = "Condition: Enrolled in Richmond Public Schools",
              choices = school_available$School)
  })
  
  filtered <- reactive({
    validate(
      need(input$age, " "),
      need(input$school, " "),
      need(input$asthma, " "),
      need(input$care, " ")
    )

      filter(decisionTree, Age == input$age,
             School == input$school,
             Asthma == input$asthma,
             Care == input$care)
  })
  output$mandatory1 <- renderUI({
    if (filtered()$`Action: Refer to RVA Breathes`== "3") {print("Refer to RVA Breathes;")} 
  })
  output$mandatory2 <- renderUI({
    if (filtered()$`Action: Refer to UCAN Program`=="3") {print("Refer to UCAN Program;")}
  }) 
  output$mandatory3 <- renderUI({
    if (filtered()$`Action: Refer to RCHD Healthy Homes`=="3") {print("Refer to RCHD Healthy Homes;")}
  }) 
  output$mandatory4 <- renderUI({
    if (filtered()$`Action: Provide One-Pager`=="3") {print("Provide One-Pager;")}
  })
  output$optional1 <- renderUI({
    if (filtered()$`Action: Refer to RVA Breathes`== "2") {print("Refer to RVA Breathes;")} 
  })
  output$optional2 <- renderUI({
    if (filtered()$`Action: Refer to UCAN Program`=="2") {print("Refer to UCAN Program;")}
  }) 
  output$optional3 <- renderUI({
    if (filtered()$`Action: Refer to RCHD Healthy Homes`=="2") {print("Refer to RCHD Healthy Homes;")}
  }) 
  output$optional4 <- renderUI({
    if (filtered()$`Action: Provide One-Pager`=="2") {print("Provide One-Pager;")}
  })
  output$doNot1 <- renderUI({
    if (filtered()$`Action: Refer to RVA Breathes`== "1") {print("Refer to RVA Breathes;")} 
  })
  output$doNot2 <- renderUI({
    if (filtered()$`Action: Refer to UCAN Program`=="1") {print("Refer to UCAN Program;")}
  }) 
  output$doNot3 <- renderUI({
    if (filtered()$`Action: Refer to RCHD Healthy Homes`=="1") {print("Refer to RCHD Healthy Homes;")}
  }) 
  output$doNot4 <- renderUI({
    if (filtered()$`Action: Provide One-Pager`=="1") {print("Provide One-Pager;")}
  })
}

app<-shinyApp(ui, server)

