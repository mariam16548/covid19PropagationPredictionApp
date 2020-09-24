zipFIPSInfo <- read.csv("data/zipToFIPS.csv")
countyFIPSInfo <- read.csv("data/FIPStoCountyAbbrev.csv")
zipcodeToCounty <- function(zipcode) {
  tbl.county <- subset(zipFIPSInfo, ZIP == zipcode)
  countyFIPS <- tbl.county$COUNTY
  tbl.countyFIPS <- subset(countyFIPSInfo, FIPS == countyFIPS)
  countyName <- as.character(tbl.countyFIPS$Name)
  stateAbbreviation <- tbl.countyFIPS$State
  stateName <- state.name[match(stateAbbreviation, state.abb)]
  return(list(countyName, stateName))
}
#source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OSLU4G
