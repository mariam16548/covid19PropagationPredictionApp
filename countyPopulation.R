library(readxl)
countyPopulationStats <- read_excel("data/censusCountyPopulationsData.xlsx")
countyPopulation <- function(countyName, stateName) {
  countyName <- paste0(".", countyName, " ", "County,", " ", stateName)
  tbl.county <- subset(countyPopulationStats, County == countyName)
  tbl.county$Population #returns the county population
}

#source for data (from Census):
#https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html
