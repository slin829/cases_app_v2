DF_plots <- function(countryOFinterest, case_no){
  # function to generate the DF for ggplots
  options(stringsAsFactors = F)
  source("./functions/DF_CaseDate.R")
  source("./functions/DF_COVIDcases.R")
  source("./functions/DF_NPI.R")
  source("./functions/DF_tests.R")
  library(dplyr)
  
  # ------------------ load data -------------
  COVID <- DF_COVIDcases(countryOFinterest, case_no)
  NPI <- DF_NPI(countryOFinterest)
  TESTS <- DF_tests(countryOFinterest)
  TESTS$Date <- as.Date(TESTS$Date)
  
  #------------------- generate DF --------------------------------
  CombDF <- left_join(COVID, NPI, by= c("Date" = "DATE_IMPLEMENTED", "Country" = "COUNTRY"))
  DF <- left_join(CombDF, TESTS, by = c("Date" = "Date", "Country"= "Country"))
  DF <- DF %>%
    select(Date, Val, Day, Country, Dataset, newCase, CATEGORY, Comment, Daily_test, Total_test, pop, Test_per_1000)
 
  # REPLACING 0s in newCase that should be a NA
  newCase0 <- numeric(length = nrow(DF))
  for(i in 1:nrow(DF)){
    if(i==1){
      newCase0[i] <- NA
    }else if(is.na(DF$newCase[i])){
      newCase0[i] <- NA
    }else if(DF$newCase[i] == 0){
      if(is.na(DF$newCase[i-1]) | DF$newCase[i-1]>=100){
        newCase0[i] <- NA
      }else if(is.na(newCase0[i-1])){
        newCase0[i] <- NA
      }
    }else{
      newCase0[i] <- DF$newCase[i]
    }
  }
  DF$newCase0 <- newCase0
  # Calculate Positivity Rate
  DF$PositivityRate <- DF$newCase0/DF$Daily_test*100
  # remove error in data e.g. US tested 4 people and 24 were positive. wtf
  DF$PositivityRate[which(DF$PositivityRate > 100)] <- NA   
  
  # Remove rows is there is missing info in the latest date
  #MissingCases <- which(DF$Date== max(DF$Date) & is.na(DF$Daily_test))
  #DF <- DF[-c(MissingCases),]
  return(DF)
}
  
  