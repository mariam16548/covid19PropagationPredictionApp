library(shiny)
library(RUnit)
library(ggplot2)
library(scales)

source("riskCalculation.R")

displayColoredBox<- function(color, riskMessage){
  sidebarPanel(h3(sprintf("%s", riskMessage), align = "center"), style=sprintf("background-color: %s; height: 120px;", color), 
               width=12)  
  }

app<-shinyApp(
  ui <- fluidPage( 
                  tags$head(
                    tags$style(HTML("
  #age, #zipcode, #groupSize {
    width: 30%;
  }
"))), 
      tabsetPanel(id = "tabs", tabPanel(h5("Introduction"), htmlTemplate("www/introduction.html")),
                tabPanel(h5("Risk Calculator"), 

                        titlePanel(h2("Will your actions today likely propagate COVID-19 to your community?", align = "center"), "COVID-19 Propagation Prediction Calculator"),
                                  
                          sidebarLayout(position = "left",
                                  sidebarPanel("",  
                                         textInput("zipcode", label="Enter your zipcode.", value = 98125), 
                                         numericInput("age", label="Enter your age.", value = ""),
                                         radioButtons("masking", "Will you wear a mask?",
                                                    c("Yes", "No")),
                                         radioButtons("alcoholConsumption", "Will you be under the influence of alcohol outside of your home?",
                                                    c("Yes", "No")),
                                         numericInput("groupSize", label="How many people (outside your household) will be with you?", value = ""),
                                         actionButton("button", "See daily case count trends")),
                                                     
                                  mainPanel("",
                                         fluidRow( 
                                           uiOutput("coloredBox"),   
                                           plotOutput("histogramOfCases", width="100%", height="500px"))
                                            )  )
                              ), 
                              tabPanel(h5("Future Work"), htmlTemplate("www/futureWork.html"))
                  ) ),
  
  server <- function(input, output, session) {
    
    observeEvent(input$zipcode,{     #limits zipcode input to 5 numbers only
      if(nchar(input$zipcode)!=5)
      {
        updateTextInput(session,'zipcode',value=98125)
        showModal(modalDialog(
          title = "Error!",
          "Only 5-character entries are permitted.",
          easyClose = TRUE
        ))
      }
      if(is.na(as.numeric(input$zipcode)))
      {
        showModal(modalDialog(
          title = "Error!",
          "Only numeric values are allowed. Please try again.",
          easyClose = TRUE
        ))
      }
    }
    )
    
    getInfectionData <- reactive({
      req(input$zipcode)
      
      zipcode <- input$zipcode
      
      infectionData <- data.frame(countyLevelInfectionData(zipcode))
      infectionData
    })
    
    whichplot <- reactiveVal(TRUE)
    
    observeEvent(input$button, {
      whichplot(!whichplot())
    })
    
    which_graph <- reactive({
      if (whichplot()) {
        infectionData<-getInfectionData()
        infectionData$weekDate <- as.Date(infectionData$weekDate, "%m/%d/%Y")
        ggplot(infectionData, aes(x=weekDate, y=caseCount)) +
          labs(title="COVID-19 Cumulative Case Count in Your Area", x="Dates since 01/22/2020", y = "Infection Count") +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) +
          theme(plot.title = element_text(size=16)) +
          geom_bar(stat="identity", color="red", fill="white") +
          scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%m/%d/%y"))     
      } else {
        infectionData<-getInfectionData()
        infectionData$weekDate <- as.Date(infectionData$weekDate, "%m/%d/%Y")
        ggplot(infectionData, aes(x=weekDate, y=currentCaseCount)) +
          labs(title="COVID-19 Daily Case Count in Your Area", x="Dates since 01/22/2020", y = "Daily Infection Count") +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) +
          theme(plot.title = element_text(size=16)) +
          geom_bar(stat="identity", color="red", fill="white") +
          scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%m/%d/%y"))
      }
    })
    
    observeEvent(input$button, {
      if (input$button %% 2 == 1) {
        txt <- "See cumulative case count trends"
      } else {
        txt <- "See daily case count trends"
      }
      updateActionButton(session, "button", label = txt)
    })
    
    getRiskAndColor<-reactive({ 
      req(input$zipcode)
      req(input$age)
      req(input$masking)
      req(input$groupSize)
      req(input$alcoholConsumption)
      
      zipcode <- input$zipcode
      age <- input$age
      masking <- input$masking
      groupSize <- input$groupSize
      alcoholConsumption <- input$alcoholConsumption
      
      likelihoodOfHarm<- riskCalculation(zipcode, masking, age, groupSize, alcoholConsumption)
      
      #designing the coloredBox
      if (likelihoodOfHarm>.85) {
        color<-"red"
        riskMessage<-"Extreme risk, stay home!"
        
      } else if (likelihoodOfHarm>.65){
        color<-"orange"
        riskMessage<-"Very high risk, stay home!"
      }
      else if (likelihoodOfHarm>.35){
        color<-"yellow"
        riskMessage<-"High risk, be careful!"
      }
      else if (likelihoodOfHarm>.10){
        color<-"#4d94ff" #this shade of blue isn't too dark
        riskMessage<-"Moderate risk, be careful!"
      } else {
        color<-"#bfff80" #this shade of green isn't too dark
        riskMessage<-"Low risk, but still be careful!"
      }
      list(color=color, riskMessage=riskMessage, likelihoodOfHarm=likelihoodOfHarm) 
      # making these variables the result from the getRiskAndColor() function as global variables, not local ones
    })
    
    output$coloredBox<-renderUI({
      riskAndColor<-getRiskAndColor() #the list/result of the function goes into a variable called riskAndColor
      displayColoredBox(riskAndColor$color, riskAndColor$riskMessage) #extract certain elements from the list to plug into the displayColoredBox() function
      #the output will be the output of the displayColorBox() function
    })
    
    output$histogramOfCases <- renderPlot({
      which_graph()
    })
    
  })

testServer(app, {
  session$setInputs(zipcode = 98125) #set the zipcode as 98125
  session$setInputs(age = 20) #set the age as 20
  session$setInputs(masking = "Yes") #set input as wearing a mask
  session$setInputs(groupSize = 1) #set the group size as 1 
  session$setInputs(alcoholConsumption = "No")  #set input as no alcohol consumption
  
  riskAndColor<-getRiskAndColor() #the list/result of the function goes into a variable called riskAndColor
  checkEquals(riskAndColor$likelihoodOfHarm, 0.09616751, tolerance = 10^-2)  #extract certain element from the list to check its value
  cat("Correct likelihood of harm value!\n") #if previous line is true, paste the given message 
  checkEquals(riskAndColor$riskMessage, "Low risk, but still be careful!") #extract certain element from the list to check its value
  cat("Correct risk message!\n") #if previous line is true, paste the given message 
  checkEquals(riskAndColor$color, "#bfff80") #extract certain element from the list to check its value
  cat("Correct color!\n") #if previous line is true, paste the given message 
})
