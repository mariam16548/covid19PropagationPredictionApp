suppressWarnings(suppressMessages(library(shiny)))
suppressWarnings(suppressMessages(library(RUnit)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(scales)))
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(data.table)))

suppressWarnings(suppressMessages(source("riskCalculation.R")))
suppressWarnings(suppressMessages(source("riskGauge.R")))

ui <- suppressWarnings(suppressMessages(fluidPage(
  tags$head(tags$style(
    HTML("
  #zipcode, #groupSize {
    width: 20%;
  }
")
  )),
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  tabsetPanel(
    id = "tabs",
    tabPanel(h5("Introduction"), htmlTemplate("www/introduction.html")),
    tabPanel(
      h5("Risk Calculator"),
      
      titlePanel(
        h2(
          "Will your actions today likely propagate COVID-19 to your community?",
          align = "center"
        ),
        "COVID-19 Propagation Prediction Calculator"
      ),
      
      sidebarLayout(
        position = "left",
        sidebarPanel(
          "",
          textInput("zipcode", label =
                      "Enter your zipcode.", value = "98125"),
          
          radioButtons("age", "Select your age bracket.",
                       c("under 30", "30 or over")),
          
          radioButtons("masking", "Will you wear a mask?",
                       c("Yes", "No")),
          radioButtons(
            "air",
            "Will you be outdoors or indoors?",
            c("outdoors", "indoors")
          ),
          radioButtons(
            "alcoholConsumption",
            "Will you be under the influence of alcohol outside of your home?",
            c("Yes", "No")
          ),
          numericInput("groupSize", label =
                         "How many people (outside your household) will be with you?", value = ""),
          actionButton("button", "See current case count trends")
        ),
        
        mainPanel("",
                  fluidRow(
                    plotOutput("riskGauge", height = "200px"),
                    plotOutput("histogramOfCases", width =
                                 "100%", height = "450px")
                  ))
      )
    ),
    tabPanel(h5("Future Work"), htmlTemplate("www/futureWork.html"))
  )
)))

server <- function(input, output, session) {
  groupSize <- reactive(input$groupSize)
  groupSizeInput <- debounce(groupSize, 1500)
  
  getInfectionData <- reactive({
    req(nchar(input$zipcode) == 5)
    zipcode <- input$zipcode
    
    infectionData <-
      suppressWarnings(suppressMessages(data.frame(countyLevelInfectionData(zipcode))))
  })
  
  whichplot <- reactiveVal(TRUE)
  
  observeEvent(input$button, {
    whichplot(!whichplot())
  })
  
  which_graph <- reactive({
    if (whichplot()) {
      infectionData <-
        suppressWarnings(suppressMessages(getInfectionData()))
      infectionData$weekDate <-
        as.Date(infectionData$weekDate, "%m/%d/%Y")
      
      date <-
        suppressWarnings(suppressMessages(infectionData$weekDate))
      caseCount <-
        suppressWarnings(suppressMessages(infectionData$caseCount))
      
      infectionData <- data.table(infectionData)
      
      ggplot(infectionData, aes(x = date, y = caseCount)) +
        labs(title = "COVID-19 Cumulative Case Count in Your County", y = "Infection Count") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text = element_text(size = 10),
              axis.title = element_text(size = 14)) +
        theme(plot.title = element_text(size = 16)) +
        geom_bar(stat = "identity",
                 color = "white",
                 fill = "red") +
        scale_x_date(NULL, date_labels = "%b %d", breaks = "2 weeks")
    } else {
      dailyInfectionData <-
        suppressWarnings(suppressMessages(getInfectionData()))
      
      dailyInfectionData$weekDate <-
        as.Date(dailyInfectionData$weekDate, "%m/%d/%Y")
      
      dailyInfectionData$weekDate <-
        as.Date(dailyInfectionData$weekDate, "%m/%d/%Y")
      
      date <-
        suppressWarnings(suppressMessages(dailyInfectionData$weekDate))
      
      currentCaseCount <-
        suppressWarnings(suppressMessages(dailyInfectionData$currentCaseCount))
      
      dailyInfectionData <- data.table(dailyInfectionData)
      
      ggplot(dailyInfectionData, aes(x = date, y = currentCaseCount)) +
        labs(title = "COVID-19 Curent Case Count in Your County", y = "Daily Infection Count") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text = element_text(size = 10),
              axis.title = element_text(size = 14)) +
        theme(plot.title = element_text(size = 16)) +
        geom_bar(stat = "identity",
                 color = "white",
                 fill = "red") +
        scale_x_date(NULL, date_labels = "%b %d", breaks = "2 weeks")
    }
  })
  
  observeEvent(input$button, {
    if (input$button %% 2 == 1) {
      buttonText <- "See cumulative case count trends"
    } else {
      buttonText <- "See current case count trends"
    }
    updateActionButton(session, "button", label = buttonText)
  })
  
  getRisk <- reactive({
    req(nchar(input$zipcode) == 5)
    req(input$age)
    req(input$air)
    req(input$masking)
    req(input$alcoholConsumption)
    req(groupSizeInput() >= 0)
    
    zipcode <- input$zipcode
    age <- input$age
    air <- input$air
    masking <- input$masking
    alcoholConsumption <- input$alcoholConsumption
    
    likelihoodOfHarm <-
      riskCalculation(zipcode,
                      masking,
                      age,
                      air,
                      groupSizeInput(),
                      alcoholConsumption)
    
    list(likelihoodOfHarm = likelihoodOfHarm)
    # making likelihood variable the result from the getRisk() function as global variables, not local ones
  })
  
  output$riskGauge <- renderPlot({
    risk <-
      suppressWarnings(suppressMessages(getRisk())) #the list/result of the function goes into a variable called risk
    riskGauge(risk$likelihoodOfHarm * 100) #extract certain elements from the list to plug into the riskGauge() function
    #the output will be the output of the riskGauge() function
  })
  
  output$histogramOfCases <- renderPlot({
    suppressWarnings(suppressMessages(which_graph()))
  })
} # server

# runApp(shinyApp(ui, server), port=9012)
shinyApp(ui, server)
