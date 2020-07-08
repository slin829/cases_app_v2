load_DF_Tests <- function(){
  # test performed
  options(stringsAsFactors = F)
  source("./functions/checkdataDate.R")
  
  # Read Tests performed per day and clean
  dataset_Date <- checkdataDate("./datasets/COVID_tests")
  TestDF <-  read.csv(paste0("./datasets/COVID_tests/", dataset_Date, "/full-list-covid-19-tests-per-day.csv" ))
  TestDF$Date <- sub(",", "-", TestDF$Date)
  TestDF$Date <- sub(" ", "-", TestDF$Date)
  TestDF$Date <- sub(" ", "", TestDF$Date)
  TestDF$Date <- as.Date(TestDF$Date, "%b-%d-%Y" )
  colnames(TestDF) <- c("Country", "Code", "Date", "Daily_test")
  
  # Read Tests performed per day and clean
  PopulationDF <- read.csv("./datasets/World_population/population_by_country_2020.csv")
  colnames(PopulationDF) <- c("Country", "Population_2020", "Yearly_Change","Net_changes","Density", "LandArea",
                              "Migrants", "Fert_Rate", "MedianAge", "Urban_pop", 'World_Share')
  # Match country names in PopulationDF and TestDF
  TestCountry <- names(table(TestDF$Country))
  PopulationCountry <- names(table(PopulationDF$Country))
  # Replace names in TestCountry that don't match Population
  which(TestCountry %in% PopulationCountry ==F)
  TestCountry[c(9, 19, 45)]
  TestDF$Country[which(TestDF$Country=="Czech Republic")] <- "Czech Republic (Czechia)"
  TestDF$Country[which(TestDF$Country=="India, people tested")] <- "India"
  TestDF$Country[which(TestDF$Country=="United States, specimens tested (CDC)")] <- "United States"
  
  avail_countries <- names(table(TestDF$Country))
  
  FinalDF <- vector()
  for(c in avail_countries){
    subDF <- subset(TestDF, TestDF$Country == c)
    subDF$Total_test <- cumsum(subDF$Daily_test)
    FinalDF <- rbind(FinalDF, subDF)
  }
  
  # Add population
  pop <- numeric(nrow(FinalDF))
  for(i in 1:nrow(FinalDF)){
    index <- which(PopulationDF$Country == FinalDF$Country[i])
    if(length(index) == 1){
      pop[i] <- PopulationDF[index,"Population_2020"]
    }else{
      pop[i] <- NA
    }
    FinalDF$pop <- pop
  }
  
  FinalDF$Test_per_1000 <- FinalDF$Total_test/FinalDF$pop*1000
  return(FinalDF)
  
}
