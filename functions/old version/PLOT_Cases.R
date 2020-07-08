
PLOT_cases <- function(countryOFinterest, case_no, curveTOplot, categoryOFinterest, plot_type){
  options(stringsAsFactors = F)
  source("./functions/DF_plots.R")
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  
  # function to load and plot data
  ## 1. cases vs Day
  ## 2. New cases vs Day
  ## 3. positivity rate vs Day
  ## 4. New positivity rate vs Day
  
  # ----------------- Variables start --------------------------
#  countryOFinterest <- c("Norway", "Sweden","New Zealand", "Italy")
#  categoryOFinterest <- c("Lockdown") #c("Lockdown", "Movement restrictions", "Public health measures", Social and economic measures" ,  "Social distancing")
#  curveTOplot <- c("Confirmed") #Death #Recovery 
#  case_no <- 1
#  plot_type <- "NewCase" # NewCase, TotalCase
  # ----------------- Variables end ---------------------------
  DF <- DF_plots(countryOFinterest, case_no)
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

  # -------------- plotting --------------------------
  p <- ggplot() + 
    scale_x_continuous(minor_breaks = seq(0, max(DF$Day) + 5, 5)) +
    theme(
      axis.text = element_text(size=14),
      axis.title = element_text(size = 14),
            legend.position = "none")
  
  if(plot_type == "TotalCase"){
    labelDF <- DF_labelDF("Val")
    ## plot case curve
    p <- p + 
      labs(
        x = paste0("Days since ", case_no, " confirmed case/s"),
        y = "Total Number of Cases"
      ) +
      scale_y_continuous(trans='log10', labels = scales::comma_format()) +
      geom_line(data= subset(DF, DF$Dataset == curveTOplot), 
                aes(x= Day, y= Val, colour=Country),
                size =1,
                na.rm = T) + 
      geom_point(data = subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest),
                 aes(x= Day, y= Val, colour=Country),
                 size=3,
                 na.rm = T
      ) +
      geom_point(data = subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest & is.na(DF$Comment)==F),
                 aes(x= Day, y= Val, colour=Country),
                 size = 4,
                 na.rm = T
      ) +
      ggrepel::geom_text_repel(data = subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest & is.na(DF$Comment)==F),
                               aes(x= Day, y= Val, label = Comment, colour=Country) ,
                               size = 5,
                               na.rm = T
      ) +
      geom_label(data = subset(labelDF, labelDF$Dataset == curveTOplot), 
                 aes(x = Day, y = Val, label = Country ,colour = Country),
                 size = 5,
                 na.rm = T
      ) 
  }else if(plot_type == "NewCase"){
    labelDF <- DF_labelDF("newCase0")
    ## plot new case
    p <- p + 
      labs(
        x = paste0("Days since ", case_no, " confirmed case/s"),
        y = "No. of New Cases"
      ) +
      scale_y_continuous(trans='log10', labels = scales::comma_format()) + 
      geom_line(data= subset(DF, DF$Dataset == curveTOplot), 
                aes(x= Day, y= newCase0, colour=Country),
                size =1,
                na.rm = T) + 
      geom_point(data = subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest),
                 aes(x= Day, y= newCase0, colour=Country),
                 size=3,
                 na.rm = T
      ) +
      geom_point(data = subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest & is.na(DF$Comment)==F),
                 aes(x= Day, y= newCase0, colour=Country),
                 size = 4,
                 na.rm = T
      ) +
      ggrepel::geom_text_repel(data = subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest & is.na(DF$Comment)==F),
                               aes(x= Day, y= newCase0, label = Comment, colour=Country),
                               size = 5,
                               na.rm = T) +
      geom_label(data = subset(labelDF, labelDF$Dataset == curveTOplot), 
                 aes(x = Day, y = newCase, label = Country ,colour = Country),
                 size = 5,
                 na.rm = T
                 ) 
  }else if(plot_type == "PositivityRate"){
    labelDF <- DF_labelDF("Daily_test")
    if(nrow(labelDF)==0){
      stop("No data available")
    }else{
      if(curveTOplot == "Confirmed"){
        ## plot positivity rate
        p <- p + 
          labs(
            x = paste0("Days since ", case_no, " confirmed case/s"),
            y = "Positivity Rate (%)"
          ) +
          scale_y_continuous(labels = scales::comma_format()) +
          geom_line(data= subset(DF, DF$Dataset == curveTOplot), 
                    aes(x= Day, y= PositivityRate, colour=Country),
                    size =1,
                    na.rm = T) + 
          geom_point(data = subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest),
                     aes(x= Day, y= PositivityRate, colour=Country),
                     size = 3,
                     na.rm = T) +
          geom_point(data = subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest & is.na(DF$Comment)==F),
                     aes(x= Day, y= PositivityRate, colour=Country),
                     size = 4,
                     na.rm = T) +
          ggrepel::geom_text_repel(data = subset(DF, DF$Dataset == curveTOplot & DF$CATEGORY == categoryOFinterest & is.na(DF$Comment)==F),
                                   aes(x= Day, y= PositivityRate, label = Comment, colour=Country),
                                   size = 5,
                                   na.rm = T) +
          geom_label(data = subset(labelDF, labelDF$Dataset == curveTOplot), 
                     aes(x = Day, y = PositivityRate, label = Country ,colour = Country),
                     size = 5,
                     na.rm = T) 
      }else{
        stop("Positivity Rate can only be ploted on Confirmed dataset")
      }
    }
  }
  
  return(p)
}
