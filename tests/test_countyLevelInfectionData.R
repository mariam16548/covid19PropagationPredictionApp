library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
source("countyLevelInfectionData.R")
source("zipcodeToCounty.R")
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
  test_countyLevelInfectionData()
} # runTests

#------------------------------------------------------------------------------------------------------------------------
test_countyLevelInfectionData <- function()
{
  message(sprintf("--- test_countyLevelInfectionData"))
  zip <- 98133
  startDate <-"01/22/2020"
  endDate <- "08/27/2020"
  countyStateNames <- zipcodeToCounty(zip)
  countyName <- countyStateNames[[1]]
  stateName <- countyStateNames[[2]]
  urlCountyInfectionStats <- paste0("https://knowi.com/api/data/ipE4xJhLBkn8H8jisFisAdHKvep",
                                    "FR5I4bGzRySZ2aaXlJgie?entityName=County%207%20day%20growth",
                                    "%20rates&exportFormat=csv&c9SqlFilter=select%20*%20where",
                                    "%20County%20like%20", 
                                    gsub(" " , "%20", countyName), 
                                    "%20County%20AND%20State%20like%20", 
                                    gsub(" " , "%20", stateName))
  tbl.countyStats <- content(GET(urlCountyInfectionStats), type = 'text/csv')
  tbl.countyStats <- data.frame(subset(tbl.countyStats, Type!="Deaths")) #only has cases not death stats
  tbl.countyStats <- data.frame(tbl.countyStats[seq(dim(tbl.countyStats)[1],1),]) #flip row order 
  tbl.countyCases <- data.frame(subset(tbl.countyStats, select = c('X7_day','X7_day_count'))) #only has date and case stats
  names(tbl.countyCases)[1] <- "weekDate"
  names(tbl.countyCases)[2] <- "caseCount"
  startDateRowNumber <- which(grepl(startDate, tbl.countyCases$weekDate)) #find row number of the startDate
  endDateRowNumber <- which(grepl(endDate, tbl.countyCases$weekDate)) #find row number of the endDate
  tbl.countyCases <- tbl.countyCases[startDateRowNumber:endDateRowNumber,,drop=F]
  kingCountyWeeklyCases <- print(data.frame(tbl.countyCases), row.names = FALSE)
  
  return(c(
    checkEquals(countyName, "King"), #check the county identification is correct
    checkEquals(stateName, "Washington"),  #check the state identification is correct
    checkEquals(nrow((subset(tbl.countyStats, Type=="Deaths"))), 0), #make sure death stats are filtered out
    checkEquals(startDateRowNumber, 1), 
    checkEquals(endDateRowNumber, 219), 
    checkEquals(class(kingCountyWeeklyCases), "data.frame"),
    checkEquals(ncol(kingCountyWeeklyCases), 2), #make sure there's only two columns
    checkEquals(tbl.countyCases$"weekDate"[1], "01/22/2020" ) #make sure the data is in chronological order
  ))
} # test_countyLevelInfectionData
#------------------------------------------------------------------------------------------------------------------------
