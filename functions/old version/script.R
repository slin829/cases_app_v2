# Does goverment intervention affect the COVID case curve?

options(stringsAsFactors = F)
library(sqldf)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(stringr)

# read uptodate data
dataset_Date <- vector()
if(!dir.exists(paste0("./outputs/tidyData/", Sys.Date()))){
  avail_dates <- as.Date(dir("./outputs/tidyData"))
  most_recent_date <- avail_dates[which(Sys.Date()- avail_dates == min(Sys.Date()-avail_dates))]
  most_recent_date <- as.character(most_recent_date)
  warning(paste0("Data not the most recent. From : ", most_recent_date))
  dataset_Date <- most_recent_date
}else{
  dataset_Date <- Sys.Date()
}
confirmed <- read.csv(paste0("./outputs/tidyData/", dataset_Date, "/confirmed.csv"))
str_sub(colnames(confirmed)[2:ncol(confirmed)], 1,1) <- ""
death <- read.csv(paste0("./outputs/tidyData/", dataset_Date, "/death.csv"))
str_sub(colnames(death)[2:ncol(death)], 1,1) <- ""
recovered <- read.csv(paste0("./outputs/tidyData/", dataset_Date, "/recovered.csv"))
str_sub(colnames(recovered)[2:ncol(recovered)], 1,1) <- ""
NPI <- read.csv(paste0("./outputs/tidyData/", dataset_Date, "/NPI.csv"))

# ------------------------ creating dataframe for plotting ---------------------------------
# Find the first confirmed case of all countries
FirstCaseDF <- vector()
for(i in 1:nrow(confirmed)){
  cases <- which(confirmed[i,] >0)
  cases <- cases[cases!= 1] #removes the first column on countries 
  first <- min(cases)
  firstDate <- colnames(confirmed)[first]
  int.restult <- data.frame(confirmed[i,1], firstDate)
  FirstCaseDF <- rbind(FirstCaseDF, int.restult)
}
FirstCaseDF$firstDate <- as.Date(FirstCaseDF$firstDate,format='%m.%d.%y')
colnames(FirstCaseDF) <- c("Country", "Date")

## case DF for plotting
plotDF <- vector()
Datasets <- list(confirmed, death, recovered)
Dataset_name <- c("Confirmed", "Death", "Recovered")
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
FinalDF <- subset(plotDF, plotDF$Country %in% countryOFinterest)

# Creating NPI_DF with goverment intervention
NPI_DF <- subset(NPI, NPI$COUNTRY %in% countryOFinterest)
NPI_DF <- subset(NPI_DF, NPI_DF$CATEGORY %in% categoryOFinterest)
NPI_DF <- data.frame(NPI_DF$DATE_IMPLEMENTED, NPI_DF$MEASURE, NPI_DF$CATEGORY, NPI_DF$TARGETED_POP_GROUP, NPI_DF$COUNTRY)
colnames(NPI_DF) <- c("DATE_IMPLEMENTED", "MEASURE", "CATEGORY", "TARGETED_GROUP", "COUNTRY")
NPI_DF$DATE_IMPLEMENTED <- as.Date(NPI_DF$DATE_IMPLEMENTED, "%d/%m/%Y")

Final_subset <- subset(FinalDF, FinalDF$Dataset == curveTOplot)
NPI_values <- vector()
for(i in 1:nrow(NPI_DF)){
  firstCaseDate <- FirstCaseDF$Date[which(FirstCaseDF$Country == NPI_DF$COUNTRY[i])]
  int.DAYS <- as.numeric(NPI_DF$DATE_IMPLEMENTED[i] - firstCaseDate +1)
  index <- which(Final_subset$Day==int.DAYS & Final_subset$Country == NPI_DF$COUNTRY[i])
  if(length(index)==0){
    int.cases <- NA
    int.result <- NA
  }else{
    int.cases <- Final_subset$Val[index]
    int.result <- data.frame(int.DAYS, int.cases, NPI_DF$COUNTRY[i])
  }
  NPI_values <- rbind(NPI_values, int.result)
}
colnames(NPI_values) <- c("DAYS", "Cases", "Country")
NPI_DF$DAYS <- NPI_values$DAYS
NPI_DF$Cases <- NPI_values$Cases
# remove rows containing NA in DAYS 
if(any(is.na(NPI_DF) == TRUE)){
  NPI_DF <- NPI_DF[-which(is.na(NPI_DF$DAYS)),]
}

label_DF <- subset(FinalDF, FinalDF$Date == max(FinalDF$Date))
graph_feature_DF <- cbind(c(col="azure4", linetype="twodash", Dataset = "Confirmed"),
                           c(col="red", linetype="dashed", Dataset = "Death"),
                           c(col="blue", linetype="dotted", Dataset = "Recovered"))
graph_feature_DF <- as.data.frame(graph_feature_DF)
colnames(graph_feature_DF) = c("Confirmed", "Death", "Recovered")


GeomPoint_feature <- vector()  
for(i in 1:nrow(NPI_DF)){
  if(NPI_DF$CATEGORY[i] == "Lockdown" & NPI_DF$MEASURE[i] == "Full lockdown" & NPI_DF$TARGETED_GROUP[i] == "No"){
    geom_cat <- "Lockdown"
    geom_size <- 3
    geom_alpha <- 1
    geom_comment <- "Full Lockdown"
  }else if(NPI_DF$CATEGORY[i] == "Lockdown" & NPI_DF$MEASURE[i] == "Partial lockdown" & NPI_DF$TARGETED_GROUP[i] == "No"){
    geom_cat <- "Lockdown"
    geom_size <- 2
    geom_alpha <- 0.6
    geom_comment <- "Partial lockdown"
  }else if(NPI_DF$CATEGORY[i] == "Social distancing"  & NPI_DF$MEASURE[i] == "Schools closure?"){
    geom_cat <- "Social distancing"
    geom_size <- 2
    geom_alpha <- 0.6
    geom_comment <- ""
  }else if(NPI_DF$CATEGORY[i] == "Movement restrictions"  & NPI_DF$MEASURE[i] == "Border closure?"){
    geom_cat <- "Movement restrictions"
    geom_size <- 3
    geom_alpha <- 1
    geom_comment <- "Border closure ?"
  }else if(NPI_DF$CATEGORY[i] == "Movement restrictions"  & NPI_DF$MEASURE[i] == "Complete border closure"){
    geom_cat <- "Movement restrictions"
    geom_size <- 3
    geom_alpha <- 1
    geom_comment <- "Complete border closure"
  }else if(NPI_DF$CATEGORY[i] == "Public health measures"  & NPI_DF$MEASURE[i] == " Mass population testing"){
    geom_cat <- "Public health measures"
    geom_size <- 3
    geom_alpha <- 1
    geom_comment <- "Mass population testing"
  }else if(NPI_DF$CATEGORY[i] == "Public health measures"  & NPI_DF$MEASURE[i] == " Health screenings in airports and border crossings"){
    geom_cat <- "Public health measures"
    geom_size <- 2
    geom_alpha <- 0.6
    geom_comment <- ""
  }else{
    geom_cat <- NA
    geom_size <- 2
    geom_alpha <- 0.4
    geom_comment <- ""
  }
  int.result <- data.frame(geom_cat, geom_size, geom_alpha, geom_comment)
  GeomPoint_feature <- rbind(GeomPoint_feature, int.result)
}
NPI_DF <- data.frame(NPI_DF, GeomPoint_feature)

# -------------------------- plot case curve and NPI --------------------------
# plot base ggplot
p <- ggplot() + 
      labs(
        x ="Day since the first confirmed case",
        y = "Number of cases",
        colour = "Legend"
      ) +
      scale_x_continuous(minor_breaks = seq(0, max(NPI_DF$DAYS), 5)) +
      scale_y_continuous(trans='log10', labels = scales::comma_format()) +
      theme(legend.position = "none")

# plot case curve
p <- p + 
      geom_line(data= subset(FinalDF, FinalDF$Dataset == curveTOplot), 
            aes(x= Day, y= Val, colour=Country),
            size =1) + 
      geom_text(data = subset(label_DF, label_DF$Dataset == curveTOplot), aes(x = Day, y = Val, label=Country), 
            size = 3, colour = graph_feature_DF["col",curveTOplot], nudge_y = 0) +
      geom_point(data = NPI_DF,
                 aes(x=DAYS, y= Cases, colour = COUNTRY), 
                 alpha = GeomPoint_feature$geom_alpha, 
                 size = GeomPoint_feature$geom_size) +
      ggrepel::geom_text_repel(data =  NPI_DF, 
                           aes(x=DAYS, 
                               y= Cases, 
                               label = geom_comment, 
                               col = COUNTRY
                           ),
                           size = 3
  )

p


