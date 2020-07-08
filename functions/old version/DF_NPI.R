DF_NPI <- function(countryOFinterest){
  source("./functions/checkdataDate.R")
  dataset_Date <- checkdataDate("outputs/tidyData")
  
  # NPI data
  NPI <- read.csv(paste0("./outputs/tidyData/", dataset_Date, "/NPI.csv"))
  # Creating NPI_DF with goverment intervention
  NPI_DF <- subset(NPI, NPI$COUNTRY %in% countryOFinterest)
  NPI_DF <- data.frame(NPI_DF$DATE_IMPLEMENTED, NPI_DF$MEASURE, NPI_DF$CATEGORY, NPI_DF$TARGETED_POP_GROUP, NPI_DF$COUNTRY)
  colnames(NPI_DF) <- c("DATE_IMPLEMENTED", "MEASURE", "CATEGORY", "TARGETED_GROUP", "COUNTRY")
  NPI_DF$DATE_IMPLEMENTED <- as.Date(NPI_DF$DATE_IMPLEMENTED, "%d/%m/%Y")
  
  avail_category <- names(table(NPI$CATEGORY))
  
  NPI_comments <- function(categoryOFinterest){
    df <- subset(NPI_DF, NPI_DF$CATEGORY %in% categoryOFinterest)
    CommentNPI <- character(length = nrow(df))
    if(nrow(df)==0){
      df <- df
    }else{
      if(categoryOFinterest == "Lockdown"){
        for(i in 1:nrow(df)){
          if(df$MEASURE[i] == "Full lockdown"){
            CommentNPI[i] <- "Full Lockdown"
          }else{
            CommentNPI[i] <- "Lockdown"
          }
          df$Comment <- CommentNPI
        }
      }
      else if(categoryOFinterest == "Movement restrictions"){
        for(i in 1:nrow(df)){
          if(df$MEASURE[i] %in% c("Border closure?", "Complete border closure")){
            CommentNPI[i] <- "Border closure"
          }else{
            CommentNPI[i] <- NA
          }
          df$Comment <- CommentNPI
        }
      }
      else if(categoryOFinterest == "Social distancing"){
        for(i in 1:nrow(df)){
          if(df$MEASURE[i] %in% c("Public services closure?")){
            CommentNPI[i] <- "Public services closure"
          }else{
            CommentNPI[i] <- NA
          }
          df$Comment <- CommentNPI
        }
      }
      else{
        for(i in 1:nrow(df)){
          CommentNPI[i] <- NA
          df$Comment <- CommentNPI
        }
      }
      return(df)
    }
  }
  
  NPI_comment_DF <- vector()
  for(c in avail_category){
    int.result <- NPI_comments(c)
    NPI_comment_DF <- rbind(NPI_comment_DF, int.result)
  }
  
  return(NPI_comment_DF)
}
