CreateDF <- function(case_no){
  options(stringsAsFactors = F)
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  library(stringr)
  
  #setwd("C:/Users/slin8/Desktop/COVID19_intervention/cases_app_v2")
  source("./functions/checkdataDate.R")
  
  dataset_Date <- checkdataDate("./datasets/COVID_dataset")
  latestFolderPath <- paste0("./outputs/tidyData/", dataset_Date)
  if(!dir.exists(latestFolderPath)){
    dir.create(latestFolderPath)
  }
  caseDF <- read.csv(paste0("./datasets/COVID_dataset/", dataset_Date, "/owid-covid-data.csv"))
  caseDF$date <- as.Date(caseDF$date, "%Y-%m-%d")
  dataset_Date <- checkdataDate("./datasets/ACAPS/csv")
  NPI_DF <- read.csv(paste0("./datasets/ACAPS/csv/", dataset_Date, "_NPI.csv"))
  NPI_DF <- NPI_DF %>%
    select("COUNTRY", "ISO", "LOG_TYPE", "CATEGORY", "MEASURE", "TARGETED_POP_GROUP", "COMMENTS", "DATE_IMPLEMENTED", "SOURCE")
  NPI_DF$DATE_IMPLEMENTED <- as.Date(NPI_DF$DATE_IMPLEMENTED, "%d/%m/%Y")
  
  DF <- left_join(caseDF, NPI_DF, by = c("iso_code" = "ISO", "date" = "DATE_IMPLEMENTED"))
  # remove ISO NA and HK
  DF <- DF[-which(DF$iso_code==""),]
  DF <- DF[-which(DF$iso_code == "HKG"),]
  
  FindFirstCaseDate <- function(iso, case_no){
    subDF <- subset(DF, DF$iso_code == iso)
    case_index <- min(which(subDF$total_cases >= case_no))
    FirstDate <- subDF$date[case_index]
    return(FirstDate)
  }
  
  countryFirstCase <- vector()
  for(c in unique(DF$iso_code)){
    int.result = FindFirstCaseDate(c, case_no)
    int.result2 <- data.frame(c, int.result)
    countryFirstCase <- rbind(countryFirstCase, int.result2)
  }
  colnames(countryFirstCase) <- c("ISO", "FirstDate")
  
  DF <- DF %>%
    left_join(countryFirstCase, by = c("iso_code" = "ISO")) %>%
    mutate(Day = as.numeric(date - FirstDate) + 1) %>%
    filter(Day >0) %>% 
    select(iso_code, location, date,FirstDate, Day, everything())
  
  write.csv(DF, paste0(latestFolderPath, "/Day_", case_no, ".csv"), row.names = F)
  
  return(DF)
  
}
