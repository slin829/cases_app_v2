options(stringsAsFactors = F)
library(ggplot2)
library(ggrepel)
library(dplyr)

setwd("C:/Users/slin8/Desktop/COVID19_intervention/cases_app_v2")
source("./functions/checkdataDate.R")
dataset_Date <- checkdataDate("outputs/tidyData")

# ------------------------------------ Read data -------------------------------------------------
# Read case datasets
confirmed <- read.csv(paste0("./datasets/COVID_timeseries/",dataset_Date, "/confirmed.csv"), check.names = F)
confirmed <- confirmed[, -which(colnames(confirmed) %in% c("Province/State", "Lat", "Long"))]
confirmed <-aggregate(.~ `Country/Region`, confirmed, sum)
death <- read.csv(paste0("./datasets/COVID_timeseries/",dataset_Date, "/death.csv"), check.names = F)
death <- death[, -which(colnames(death) %in% c("Province/State", "Lat", "Long"))]
death <-aggregate(.~ `Country/Region`, death, sum)
recovered <- read.csv(paste0("./datasets/COVID_timeseries/",dataset_Date, "/recovered.csv"), check.names = F)
recovered <- recovered[, -which(colnames(recovered) %in% c("Province/State", "Lat", "Long"))]
recovered <-aggregate(.~ `Country/Region`, recovered, sum)
# NB: separate French polynesian countries from France

# Read NPI dataset
avail_NPI_files <- dir("./datasets/ACAPS/csv")
avail_NPI_files <- sub("_NPI.csv", "", avail_NPI_files)
NPI_file_date <- as.Date(avail_NPI_files)
NPI_file_newest <- avail_NPI_files[which(Sys.Date() - NPI_file_date == min(Sys.Date() - NPI_file_date))]
NPI_file_read <- as.character(NPI_file_newest)
NPI <- read.csv(paste0("./datasets/ACAPS/csv/", NPI_file_read, "_NPI.csv"))

# ---------------------------- data cleanup ------------------------------------------------
# Clean up NPI data country name to match COVID curve dataset
NPI_names <- names(table(NPI$COUNTRY))
TS_names <- confirmed$`Country/Region`
#NPI_names[which(NPI_names %in% TS_names==F)]
#TS_names[which(TS_names %in% NPI_names==F)]
NPI$COUNTRY[which(NPI$COUNTRY == "Brunei Darussalam")] <- "Brunei"
NPI$COUNTRY[which(NPI$COUNTRY== "Russian Federation")] <- "Russia"
NPI$COUNTRY[which(NPI$COUNTRY == "North Macedonia Republic Of")] <- "North Macedonia"
NPI$COUNTRY[which(NPI$COUNTRY == "Moldova Republic of")] <- "Moldova"
NPI$COUNTRY[which(NPI$COUNTRY == "Viet Nam")] <- "Vietnam"
NPI$COUNTRY[which(NPI$COUNTRY == "United States of America")] <- "US"
NPI$COUNTRY[which(NPI$COUNTRY == "Korea Republic of")] <- "Korea, South"
NPI$COUNTRY[which(NPI$COUNTRY == "Czech Republic")] <- "Czechia"
NPI$COUNTRY[which(NPI$COUNTRY == "C?te d'Ivoire")] <- "Cote d'Ivoire"
# Remove missing data (date) from NPI 
NPI <- NPI[-which(NPI$DATE_IMPLEMENTED==""),]

NPI$CATEGORY[which(NPI$CATEGORY == "Lockdown ")] <- "Lockdown"
NPI$CATEGORY[which(NPI$CATEGORY == "Movement Restrictions ")] <- "Movement restrictions"
NPI$CATEGORY[which(NPI$CATEGORY == "Social and Economic Measures")] <- "Social and economic measures"
NPI$CATEGORY[which(NPI$CATEGORY == "Social Distancing")] <- "Social distancing"
# Remove missing data (Category) from NPI 

# ---------------- make sure population data country name matchhes TS name ----------
source("./functions/Load_DF_rawTESTdata.R")
testDF <- load_DF_Tests()
testCountryName <- names(table(testDF$Country))
testCountryName[c(which(testCountryName %in% TS_names ==F))]

testDF$Country[which(testDF$Country == "Czech Republic (Czechia)")] <- "Czechia"
testDF$Country[which(testDF$Country == "South Korea")] <- "Korea, South"
testDF$Country[which(testDF$Country == "Taiwan")] <- "Taiwan*"
testDF$Country[which(testDF$Country == "United States")] <- "US"

# ----------- write files to outputs ---------------------

if(dir.exists(paste0("./outputs/tidyData/", dataset_Date))==F){
  dir.create(paste0("./outputs/tidyData/", dataset_Date))
}

write.csv(testDF, paste0("./outputs/tidyData/", dataset_Date,"/Tests.csv"), row.names = F)
write.csv(NPI, paste0("./outputs/tidyData/", dataset_Date,"/NPI.csv"), row.names = F)
write.csv(confirmed, paste0("./outputs/tidyData/", dataset_Date,"/confirmed.csv"), row.names = F)
write.csv(death, paste0("./outputs/tidyData/", dataset_Date,"/death.csv"),row.names = F)
write.csv(recovered, paste0("./outputs/tidyData/", dataset_Date,"/recovered.csv"),row.names = F)
          