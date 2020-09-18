library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
source("riskCalculation.R")
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
  test_riskCalculation()
} # runTests

#------------------------------------------------------------------------------------------------------------------------
test_riskCalculation <- function()
{
  message(sprintf("--- test_riskCalculation"))
  
  zipcode=98125
  masking="Yes"
  age=20
  groupSize=1
  alcoholConsumption = "No"
  
  infectionData <- data.frame(countyLevelInfectionData(zipcode))
  countyPopulatonDensity<-infectionData[1, 4] #extract the county population density
  countyPopulation<- infectionData[1, 5] #extract the county population
  mostRecentCaseCount<- tail(infectionData$caseCount,1)
  likelihoodOfHarm <- (countyPopulatonDensity*mostRecentCaseCount)/100000000
  
  likelihoodOfHarm <- if_else(masking == "Yes", likelihoodOfHarm * 0.35, likelihoodOfHarm)
  likelihoodOfHarm <- if_else(age < 30, likelihoodOfHarm * 1.31, likelihoodOfHarm)
  
  
  likelihoodOfHarm <- case_when(
    groupSize > 0 ~ likelihoodOfHarm * (1+(1-((1-(mostRecentCaseCount/countyPopulation))^groupSize))), 
    groupSize == 0 ~ likelihoodOfHarm
  )
  likelihoodOfHarm <- if_else(alcoholConsumption == "Yes", likelihoodOfHarm * 1.66, likelihoodOfHarm)  
  
  
  return(cat(
    checkEquals(countyPopulatonDensity, 1022.672, tolerance=10^-4), #since the number is irrational, we can use close estimates
    checkEquals(countyPopulation, 2252782), 
    checkEquals(likelihoodOfHarm, 0.09574268, tolerance=10^-1) #this number is subject to change since it uses most recent data
  ))
} # test_riskCalculation
#------------------------------------------------------------------------------------------------------------------------
