info_plotCase <- function(countryOFinterest, case_no, curveTOplot, categoryOFinterest,
                          plot_type, x, y){
  options(stringsAsFactors = F)
  source("./functions/DF_plots.R")
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  
  # function to print Table information related to plotCases
  
  # ----------------- Variables start --------------------------
  #  countryOFinterest <- c("Norway", "Sweden","New Zealand", "Italy")
  #  categoryOFinterest <- c("Lockdown") #c("Lockdown", "Movement restrictions", "Public health measures", Social and economic measures" ,  "Social distancing")
  #  curveTOplot <- c("Confirmed") #Death #Recovery 
  #  case_no <- 1
  #  plot_type <- "NewCase" # NewCase, TotalCase
  
# working in progress
  x <- round(x)
  y <- round(y)
  # ----------------- Variables end ---------------------------
  
  DF <- DF_plots(countryOFinterest, case_no)
  
  axisMax <- DF %>%
    filter(Dataset == curveTOplot) %>%
    filter(Val == max(Val))
  ymax <- axisMax$Val
  xmax <- axisMax$Day
# working in progress  
  
  # generate plot Label
  DF_labelDF <- function(plotBY){
    # plotBy = "Daily_test" , "newCase0", "Val"
    labelDF <- vector()
    for(c in names(table(DF$Country))){
      subDF <- subset(DF, DF$Country ==c)
      RemoveNA <- which(is.na(subDF[plotBY]))
      if(length(RemoveNA) > 0){
        subDF <- subDF[-RemoveNA,]
      }else{
        subDF <- subDF
      }
      # remove rows
      if(nrow(subDF)==0){
        int.result <- subDF[0,]
      }else{
        index <- which(subDF$Date == max(subDF$Date))
        int.result <- subDF[index,]
      }
      labelDF <- rbind(labelDF, int.result)
    }
    return(labelDF)
  }
  
  if(plot_type == "TotalCase"){
    labelDF <- DF_labelDF("Val")
    subDF <- subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest)
    subDF <- subset(subDF, subDF$Day == x & subDF$Val == y)
    Cases_info <-   subDF$Val
    ## plot case curve
  }else if(plot_type == "NewCase"){
    labelDF <- DF_labelDF("newCase0")
    subDF <- subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest)
    subDF <- subset(subDF, subDF$Day == x & subDF$newCase0 == y)
    NewCases_info <-   subDF$newCase0
    ## plot new case
  }else if(plot_type == "PositivityRate"){
    labelDF <- subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest)
    if(nrow(labelDF)==0){
      stop("No data available")
    }else{
      if(curveTOplot == "Confirmed"){
        subDF <- subset(DF, DF$Dataset == curveTOplot)
        subDF <- subset(subDF, subDF$Day == x & subDF$PositivityRate == y)
        PositivityRate_info <-   subDF$PositivityRate
      }else{
        stop("Positivity Rate can only be ploted on Confirmed dataset")
      }
    }
  }
  
  Country_info <- subDF$Country
  Day_info <-     subDF$Day
  Date_info <-    subDF$Date
  NPI_info <-     subDF$Comment
  
  if(plot_type == "TotalCase"){
    outputInfo <- data.frame(Country_info, Day_info, Date_info, Cases_info, NPI_info)
  }else if(plot_type == "NewCase"){
    outputInfo <- data.frame(Country_info, Day_info, Date_info, NewCases_info, NPI_info)
  }else if(plot_type == "PositivityRate"){
    outputInfo <- data.frame(Country_info, Day_info, Date_info, PositivityRate_info, NPI_info)
  }

  outputInfo <- t(outputInfo)
  rownames(outputInfo) <- c(sub("_info", "", rownames(outputInfo)))
  outputInfo <- as.data.frame(outputInfo)
  return(outputInfo)
}
