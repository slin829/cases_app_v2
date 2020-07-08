plot_case_NPI <- function(case_no, countryOFinterest, categoryOFinterest, MeasureDisplay){
  library(ggplot2)
  library(dplyr)
  library(ggrepel)
  library(scales)
  # library(sringr)
  
  ## VAR requried
  # case_no = 1
  # countryOFinterest
  # # categoryOFinterest
  # MeasureDisplay <- c("Limit public gatherings", "Partial lockdown")
  
  #setwd("C:/Users/slin8/Desktop/COVID19_intervention/cases_app_v2")
  source("./functions/checkdataDate.R")
  source('./functions/CreateDF.R')
  
  path <- "./outputs/tidydata"
  dataset_Date <- checkdataDate(path)
  DF_path <- paste0(path,"/", dataset_Date, "/Day_", case_no, ".csv")
  if(file.exists(DF_path)){
    DF <- read.csv(paste0(path,"/", dataset_Date, "/Day_", case_no, ".csv"))
  }else{
    DF <- CreateDF(case_no)
  }
  
  DF <- DF %>%
    filter(location %in% countryOFinterest) %>%
    mutate(total_cases= replace(total_cases, total_cases ==0, NA)) # removes missing values

  # data frame from plotting labels
  labelDF <- DF %>%
    select(location, Day, total_cases, new_cases, total_deaths, new_deaths) %>%
    group_by(location)%>%
    filter(Day == max(Day))
  
  #  ------------------ plots ------------------------
  p <- ggplot() +
    scale_x_continuous(minor_breaks = seq(0, max(DF$Day) + 5, 5)) +
    scale_y_continuous(trans='log10', labels = comma_format(accuracy = 1)) +
    theme(
      axis.text = element_text(size=12),
      axis.title = element_text(size = 12),
      legend.position = "none")
  
  # plot cas and NPI
  NPI_data <- DF %>%
    filter(CATEGORY == categoryOFinterest)
  
  p <- p +
    labs(
      x = paste0("Days since ", case_no, " confirmed case/s"),
      y = "Total Number of confirmed Cases"
    ) +
    geom_line(data = DF, aes(x= Day, y = total_cases, group = location , colour = location),
              size = 1, alpha = 0.8) +
    geom_point(data = NPI_data, aes(x= Day, y = total_cases, group = location , colour = location),
               size =3, alpha= 0.8) +
    geom_label(data = labelDF, aes(x = Day, y= total_cases, label = location, colour = location),
               alpha = 0.8, nudge_x = 7)
  
  if(!is.null(MeasureDisplay)){
    NPI_data <- NPI_data %>%
      filter(MEASURE %in% MeasureDisplay)
    p <- p +
      ggrepel::geom_text_repel(data = NPI_data, aes(x= Day, y = total_cases, label = MEASURE, colour = location),
                               size = 3,
                               na.rm = T) 
  }
  return(p)
}
