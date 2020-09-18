library(readxl)
countyInfo <- read_excel("data/censusCountyData.xlsx")
countyPopulationDensity<- function(countyName, stateName) {
  countyName <- paste(countyName,"County")
  tbl.county <- subset(countyInfo, County == countyName) 
  tbl.county <- subset(tbl.county, State == stateName) 
  tbl.county[3]<- tbl.county[3]*2.58998811 #convert from people/sq.km to people/sq.miles
  tbl.county$`County Population Density` #returns the county population density 
}

#source for data (from Census): 
#https://covid19.census.gov/datasets/21843f238cbb46b08615fc53e19e0daf/data?geometry=-126.247%2C28.795%2C126.878%2C67.148&selectedAttribute=B01001_calc_PopDensity
