DF_COVIDcases <- function(countryOFinterest, case_no){
  # Transform case csv files into standard data.frame 
  options(stringsAsFactors = F)
  library(stringr)
  
  source("./functions/DF_CaseDate.R")
  source("./functions/checkdataDate.R")
  dataset_Date <- checkdataDate("outputs/tidyData")
  
  # read uptodate data
  confirmed <- read.csv(paste0("./outputs/tidyData/", dataset_Date, "/confirmed.csv"))
  str_sub(colnames(confirmed)[2:ncol(confirmed)], 1,1) <- ""
  death <- read.csv(paste0("./outputs/tidyData/", dataset_Date, "/death.csv"))
  str_sub(colnames(death)[2:ncol(death)], 1,1) <- ""
  recovered <- read.csv(paste0("./outputs/tidyData/", dataset_Date, "/recovered.csv"))
  str_sub(colnames(recovered)[2:ncol(recovered)], 1,1) <- ""
  

  plotDF <- vector()
  Datasets <- list(confirmed, death, recovered)
  Dataset_name <- c("Confirmed", "Death", "Recovered")
  csv_filename <- c("confirmed", "death", "recovered")
  FirstCaseDF <- DF_CaseDate(case_no)
  for(country in countryOFinterest){
    for(DS in 1:length(Datasets)){
      DF <- subset(Datasets[[DS]], Datasets[[DS]]$Country.Region == country)
      DF <- t(DF)
      Date <- rownames(DF)
      DF<- data.frame(Date, DF)
      colnames(DF) <- c("Date", "Val")
      DF <- DF[-1,]
      DF$Date <- as.Date(DF$Date, format = '%m.%d.%y')
      rownames(DF) = c()
      doi <- FirstCaseDF[which(FirstCaseDF$Country == country),2]
      index_firstday <- which(DF$Date == doi)
      day_vector <- c(rep(NA, index_firstday-1), seq(1, nrow(DF)-index_firstday+1, by=1))
      int.result <- data.frame(DF, day_vector, rep(country, nrow(DF)), rep(Dataset_name[DS], nrow(DF)))
      plotDF <- rbind(plotDF, int.result)
    }
  }
  colnames(plotDF) <- c("Date", "Val", "Day", "Country", "Dataset")
  plotDF$Val <- as.numeric(plotDF$Val)
  plotDF <- subset(plotDF, plotDF$Day %in% NA ==F)

  
  # add extra column for new Case counts
  DF_newCase<- function(c, DS){
    subDF <- subset(plotDF, plotDF$Country == c & plotDF$Dataset == DS)
    newCase <- numeric(nrow(subDF))
    for(i in 1:nrow(subDF)){
      if(i==1){
        newCase[i] <- NA
      }else{
        newCase[i] <- subDF$Val[i] - subDF$Val[i-1]
      }
      newsubDF <- data.frame(subDF, newCase)
    }
    return(newsubDF)
  }
  
  FinalDF <- vector()
  for(c in countryOFinterest){
    for(DS in c("Confirmed", "Death", "Recovered")){
      int.result <- DF_newCase(c, DS)
      FinalDF <- rbind(FinalDF, int.result)
    }
  }
  
  return(FinalDF)
}
