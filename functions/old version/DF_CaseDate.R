

DF_CaseDate <- function(case_no){
  source("./functions/checkdataDate.R")
  dataset_Date <- checkdataDate("outputs/tidyData")
  
  path <- paste0("outputs/tidyData/", dataset_Date)
  Dataset = "confirmed"
  DF <- read.csv(paste0(path, "/", Dataset, ".csv"), check.names = F)
  # Find the cooresponding date since the x case of all countriesa
  FirstCaseDF <- vector()
  for(i in 1:nrow(DF)){
    cases <- which(DF[i,] >= case_no)
    cases <- cases[cases!= 1] #removes the first column on countries 
    if(length(cases)>0){
      first <- min(cases)
      firstDate <- colnames(DF)[first]
    }else{
      firstDate = NA
    }
    int.restult <- data.frame(DF[i,1], firstDate)
    FirstCaseDF <- rbind(FirstCaseDF, int.restult)
  }
  FirstCaseDF$firstDate <- as.Date(FirstCaseDF$firstDate,format='%m/%d/%y')
  colnames(FirstCaseDF) <- c("Country", "Date")
  
  FirstCaseDF
}


