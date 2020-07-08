
options(stringsAsFactors = F)
source("./functions/CreateDF.R")
source("./functions/plot_case_NPI.R")
source("./functions/checkdataDate.R")

library(dplyr)
library(shiny)
library(ggrepel)

# ------------------ load data -------------
dataset_Date <- checkdataDate("./datasets/COVID_dataset")
path <- "./outputs/tidydata"
DF_path <- paste0(path,"/", dataset_Date, "/Day_", 1, ".csv")
if(file.exists(DF_path)){
  DF <- read.csv(paste0(path,"/", dataset_Date, "/Day_", 1, ".csv"))
}else{
  DF <- CreateDF(1)
}
avail_category <- unique(DF$CATEGORY)
avail_category <- avail_category[-which(is.na(avail_category))]
avail_country <- unique(DF$location)

ui <- fluidPage(
  titlePanel(h3("Explore Government Interventions on COVID-19 Case Numbers")),
  hr(),
  h5("Click on any points on the plot to display relevant information on government interventions"),
  fluidRow(
    column(width = 3, wellPanel( 
      selectizeInput("Country", "Country", choices = avail_country,selected = c("New Zealand", "Australia"), multiple = TRUE),
      selectInput("NPI", "Government Intervention", choices = avail_category, selected = avail_category[5]),
      uiOutput("Measure"),
      numericInput("case_no", "No. of cases to set as Day 1", 1)
      # radioButtons("curveTOplot", "Dataset", choices = c("Confirmed", "Death", "Recovered")),
      # radioButtons("plot_type", "Plot Type", choices = c("TotalCase", "NewCase", "PositivityRate"))
    )),
    column(width = 6,
          plotOutput("Cases", 
                     click = clickOpts("plot_click"),
                     width = 800, height = 600),
          tableOutput("hover_info")
    )
  ),
  hr(),
  h6("Data sources. Case Data: OWID, https://github.com/owid/covid-19-data/tree/master/public/data
     Government Intervention: ACAPS, https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset.
     Data obtained on 2020-07-08")
  #h6("Government Intervention: ACAPS, https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset"),
  #h6("COVID testing: Our World in Data, https://ourworldindata.org/covid-testing"),
  #h6("Data obtained on 2020-04-14")
)

server <- function(input, output){
  
  output$Measure <- renderUI({
    NPI_data <- DF %>%
      filter(CATEGORY == input$NPI) 
    avail_measures <- unique(NPI_data$MEASURE)
    checkboxGroupInput("Measure", "Display Measures", choices = avail_measures, selected = NULL)
  })
  
  output$Cases <- renderPlot({
    plot_case_NPI(input$case_no, input$Country, input$NPI, input$Measure)
  })
  
  output$hover_info <- renderTable({
    path <- "./outputs/tidydata"
    dataset_Date <- checkdataDate(path)
    DF_path <- paste0(path,"/", dataset_Date, "/Day_", input$case_no, ".csv")
    if(file.exists(DF_path)){
      DF <- read.csv(paste0(path,"/", dataset_Date, "/Day_", input$case_no, ".csv"))
    }else{
      DF <- CreateDF(input$case_no)
    }
    # creates DF to display COMMENTS
    info_DF <- DF %>%
      filter(location %in% input$Country) %>%
      filter(CATEGORY == input$NPI) %>%
      select(location, Day, date, total_cases, new_cases, total_deaths, new_deaths, CATEGORY, MEASURE, COMMENTS)

    point <- nearPoints(df = info_DF, coordinfo = input$plot_click, threshold = 5) %>%
      select(date, Day, total_cases, MEASURE, COMMENTS)
    point$date <- as.character(point$date)
    point

  }, striped = T, width = 800, digits = 0)

}

shinyApp(ui = ui, server = server)