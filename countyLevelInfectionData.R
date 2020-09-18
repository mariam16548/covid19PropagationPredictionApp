source("zipcodeToCounty.R")
source("countyPopulationDensity.R")
source("countyPopulation.R")

countyLevelInfectionData<- function(zip, startDate=NA, endDate=NA) { #dates must be written in "XX/XX/XXXX" format (make sure written as a STRING)
  countyStateNames <- zipcodeToCounty(zip)
  countyName <- countyStateNames[1]
  stateName <- countyStateNames[2]
  urlCountyInfectionStats <- paste0("https://knowi.com/api/data/ipE4xJhLBkn8H8jisFisAdHKvep",
                                    "FR5I4bGzRySZ2aaXlJgie?entityName=County%207%20day%20growth",
                                    "%20rates&exportFormat=csv&c9SqlFilter=select%20*%20where",
                                    "%20County%20like%20", 
                                    gsub(" " , "%20", countyName), 
                                    "%20County%20AND%20State%20like%20", 
                                    gsub(" " , "%20", stateName))
  tbl.countyStats <- content(GET(urlCountyInfectionStats), type = 'text/csv')
  if (length(tbl.countyStats)==0) {
    print("The selected zipcode is invalid. Please try again.")
  }  else {
    tbl.countyCases <- subset(tbl.countyStats, Type!="Deaths") #only has cases not death stats
    tbl.countyCases <- tbl.countyCases[seq(dim(tbl.countyCases)[1],1),] #flip row order 
    tbl.countyCases <- subset(tbl.countyCases, select = c('7_day','7_day_count')) #only has date and case stats
    names(tbl.countyCases)[1] <- "weekDate"
    names(tbl.countyCases)[2] <- "caseCount"
    tbl.countyCases$currentCaseCount <- c(tbl.countyCases$caseCount[1],diff(tbl.countyCases$caseCount))
    
    if (is.na(startDate)) {  #check if the startDate or endDates are NA-- defaults to giving earliest/latest data, respectively 
      startDate <- tbl.countyCases$weekDate[1]
    } else {
      startDate <- startDate
    }
    
    if (is.na(endDate)) {
      endDate<- tail(tbl.countyCases$weekDate,1)
    } else {
      endDate <- endDate
    }
    
    startDateRowNumber <- which(grepl(startDate, tbl.countyCases$weekDate)) #find row number of the startDate
    endDateRowNumber <- which(grepl(endDate, tbl.countyCases$weekDate)) #find row number of the endDate
    if (length(startDateRowNumber)==0 || length(endDateRowNumber)==0) {
      print("The dates you selected are not within the available range, please select a start date that is 01/22/2020 or later.")
    }  else {
      startDateRowNumber <- startDateRowNumber
      tbl.countyCases <- tbl.countyCases[startDateRowNumber:endDateRowNumber,,drop=F]
      tbl.countyCases$countyPopulationDensity<- countyPopulationDensity(countyName, stateName) #create a new column with pop density
      tbl.countyCases$countyPopulation<- countyPopulation(countyName, stateName) #create a new column with pop 
      print(data.frame(tbl.countyCases), row.names = FALSE)
      
    }
  }
}
