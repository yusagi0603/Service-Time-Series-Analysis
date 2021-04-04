library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(forecast)
library(DT)
library(plotly)
library(lubridate)
library(dplyr)
library(tidyr)

all_unique_service <- c("洗髮", "染髮", "燙髮", "剪髮", "護髮", "養護")
all_unique_store <- readr::read_csv("./all_unique_store.csv")$store

fluidPage(
  titlePanel("Happy Hair Service Demand Forcast Dashboard"),
  
  ##### SELECT STORE & SERVICE #####
  fluidRow(
    column(4,
           selectInput("store1",
                       "Store1:",
                       c(unique(as.character(all_unique_store)))) # remove ALL
    ),
    column(4,
           selectInput("store2",
                       "Store2:",
                       c(unique(as.character(all_unique_store)))) # remove ALL
    ),
    column(4,
           selectInput("store3",
                       "Store3:",
                       c(unique(as.character(all_unique_store)))) # remove ALL
    ),
    column(4,
           selectInput("service",
                       "Service:",
                       c("All",
                         unique(as.character(all_unique_service))))
    ),
  ),
  
  ###### DATE RANGE INPUT #####
  column(4, wellPanel(
    dateRangeInput('dateRange',
                   label = 'Date range input: yyyy-mm-dd',
                   start = as.Date("2020-03-15"), end = as.Date("2020-08-14")
    ),
  )),
  
  column(6,
         verbatimTextOutput("dateRangeText"),
         verbatimTextOutput("storeText"),
         verbatimTextOutput("serviceText"),
         verbatimTextOutput("groupText")
  ),
  mainPanel(
    plotlyOutput("profilePlot", width="1200px",height="600px")
  ),  
  mainPanel(
    plotlyOutput("timeSeriesPlot", width="1200px",height="600px")
  ),
  
  DT::dataTableOutput("demandTable")
)