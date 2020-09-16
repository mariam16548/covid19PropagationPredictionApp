library(shiny)
library(RUnit)

source("riskCalculation.R")

displayColoredBox<- function(color, riskMessage){
  sidebarPanel(style=sprintf("background-color: %s; width: 550px; height: 120px;", color),
               h3(sprintf("%s", riskMessage), align = "center") )  
}
app <- shinyApp(
  ui <- fluidPage(   
    tabsetPanel(id = "tabs",
            tabPanel(h5("Risk Calculator"), 
                            
    
    titlePanel("Will your actions today likely propagate COVID-19 to your community?"),
    
    sidebarLayout(position = "left",
                  sidebarPanel("",
                               textInput("zipcode", label="Enter your zipcode.", value = 98125), 
                               numericInput("age", label="Enter your age.", value = ""),
                               numericInput("groupSize", label="How many people (outside your household) will be with you?", value = ""),
                               radioButtons("masking", "Will you wear a mask?",
                                           c("Yes", "No")),
                               radioButtons("alcoholConsumption", "Will you be under the influence of alcohol outside of your home?",
                                           c("Yes", "No"))),
                  
                  mainPanel("",
                            fluidRow(
                              uiOutput("coloredBox"), 
                              plotOutput("histogramOfCases", width="100%", height="500px")), 
                            
                  ))
    )
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
      if (likelihoodOfHarm>1) {
        color<-"red"
        riskMessage<-"Extreme risk, stay home!"
        
      } else if (likelihoodOfHarm>.75){
        color<-"orange"
        riskMessage<-"Very high risk, stay home!"
      }
      else if (likelihoodOfHarm>.45){
        color<-"yellow"
        riskMessage<-"High risk, be careful!"
      }
      else if (likelihoodOfHarm>.20){
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
      infectionData<-getInfectionData() 
      plot(1:nrow(infectionData), infectionData$caseCount, main="COVID-19 Case Count in Your Area", 
           xlab="Days since 01/22/2020", ylab="Infection Count", cex.main=1.5, cex.lab=1.3, cex.axis=1.2, col="red", "h")
    })
  }
)

testServer(app, {
  session$setInputs(zipcode = 98125) #set the zipcode as 98125
  session$setInputs(age = 20) #set the age as 20
  session$setInputs(masking = "Yes") #set input as wearing a mask
  session$setInputs(groupSize = 1) #set the group size as 1 
  session$setInputs(alcoholConsumption = "No")  #set input as no alcohol consumption
  
  riskAndColor<-getRiskAndColor() #the list/result of the function goes into a variable called riskAndColor
  checkEquals(riskAndColor$likelihoodOfHarm, 0.09616751, tolerance = 10^-7)  #extract certain element from the list to check its value
  cat("Correct likelihood of harm value!\n") #if previous line is true, paste the given message 
  checkEquals(riskAndColor$riskMessage, "Low risk, but still be careful!") #extract certain element from the list to check its value
  cat("Correct risk message!\n") #if previous line is true, paste the given message 
  checkEquals(riskAndColor$color, "#bfff80") #extract certain element from the list to check its value
  cat("Correct color!\n") #if previous line is true, paste the given message 
})
