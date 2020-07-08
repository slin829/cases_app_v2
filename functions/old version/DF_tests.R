DF_tests <- function(countryOFinterest){
  source("./functions/checkdataDate.R")
  dataset_Date <- checkdataDate("outputs/tidyData")
  
  path <- paste0("outputs/tidyData/", dataset_Date)
  Dataset = "Tests"
  DF <- read.csv(paste0(path, "/", Dataset, ".csv"), check.names = F)
  
  subDF <- subset(DF, DF$Country %in% countryOFinterest)
  return(subDF)
}
