checkdataDate <- function(folder){
  # folder = "outputs/tidyData"
  
  # check availavle datasets date and return dataset_Date 
  if(dir.exists(paste0(folder, "/", Sys.Date()))){
    dataset_Date <- Sys.Date()
  }else{
    avail_dates <- as.Date(dir(folder))
    most_recent_date <- avail_dates[which(Sys.Date()- avail_dates == min(Sys.Date()-avail_dates))]
    most_recent_date <- as.character(most_recent_date)
    #warning(paste0("Data not the most recent. From : ", most_recent_date))
    dataset_Date <- most_recent_date
  }
  return(dataset_Date)
}