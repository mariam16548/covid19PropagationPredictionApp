library(httr) 
urlCounties <- "https://raw.githubusercontent.com/scpike/us-state-county-zip/master/geo-data.csv"
zipcodeToCountyData <- content(GET(urlCounties), type = 'text/csv')

zipcodeToCounty<- function(zip) {
    tbl.county <- subset(zipcodeToCountyData, zipcode == zip) 
    
      countyName <- tbl.county$county
      if (length(countyName)==0) {
        print("The selected zipcode is invalid. Please try again.")
        
      }  else {
        stateName <- tbl.county$state
        return(list(countyName, stateName))
      }
}
