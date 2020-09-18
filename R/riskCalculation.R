library(dplyr)
source("R/countyLevelInfectionData.R")

riskCalculation<- function(zipcode, masking, age, groupSize, alcoholConsumption) {
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
  
  return(likelihoodOfHarm)
}

#source for mask lowering risk calculation: 
  #https://www.ucdavis.edu/coronavirus/news/your-mask-cuts-own-risk-65-percent/

#source for alcohol causing lowered awareness and increasing risk calculation: --- .08 BAC results in avg of 66% impairment
  #https://one.nhtsa.gov/people/injury/research/pub/HS809028/index_.htm

#source for younger people more likely to exhibit risky behavior: 
  #https://komonews.com/news/coronavirus/troubling-trend-shows-more-young-people-testing-positive-for-covid-19
  #https://www.voterstudygroup.org/covid-19-updates
  #45% of people under 30 are socializing with people they do not live with and not social distancing
  #around 40% of COVID cases are from people under 30
  #therefore, around 16% of young people (likely asymptomatic) are engaging in risky behavior
  #82% risk reduction when social distancing (https://newsroom.unsw.edu.au/news/health/covid-19-how-much-do-social-distancing-and-masks-reduce-risk)
  #1-(.82*(1-.16))=.31-> 31% risk increase

#source for groupSize calculation: 
  #https://covid19risk.biosci.gatech.edu/
