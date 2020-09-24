zipFIPSInfo <- read.csv("data/zipToFIPS.csv")
zipFIPSInfo$ZIP<-sapply(zipFIPSInfo$ZIP, function(x){if(nchar(x)==3){paste0(0,0,x)}else{x}})
zipFIPSInfo$ZIP<-sapply(zipFIPSInfo$ZIP, function(x){if(nchar(x)==4){paste0(0,x)}else{x}})

countyFIPSInfo <- read.csv("data/FIPStoCountyAbbrev.csv")

zipcodeToCounty <- function(zipcode) {
  zipcode<-sapply(zipcode, function(x){if(nchar(x)==3){paste0(0,0,x)}else{x}})
  zipcode<-sapply(zipcode, function(x){if(nchar(x)==4){paste0(0,x)}else{x}})
  tbl.county <- subset(zipFIPSInfo, ZIP == zipcode)
  countyFIPS <- tbl.county$COUNTY
  tbl.countyFIPS <- subset(countyFIPSInfo, FIPS == countyFIPS)
  countyName <- as.character(tbl.countyFIPS$Name)
  stateAbbreviation <- tbl.countyFIPS$State
  stateName <- state.name[match(stateAbbreviation, state.abb)]
  return(list(countyName, stateName))
}
#source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OSLU4G
