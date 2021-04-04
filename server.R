## load packages
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
# library(highcharter)

# input$[variable] to use variable in ui.R
# EX: input$store -> to use a variable "store" in ui.R
###############
source("./data_source.R")
source("./feature_engineering.R")
###############

function(input, output, session) {
  
  ##### PARSE VARIABLE FROM ui.R #####
  # input$date and others are Date objects. When outputting
  # text, we need to convert to character; otherwise it will
  # print an integer rather than a date.
  
  ##### Output text & plot ######
  
  output$dateRangeText  <- renderText({
    paste("Time Range: ", 
          paste(as.character(input$dateRange), collapse = " to ")
    )
  })
  output$storeText <- renderText({
    paste0("Store: ",
      paste0(c(input$store1, input$store2, input$store3), collapse=", ")
    )
  })
  output$serviceText <- renderText({
    paste0("Service: ", input$service)
  })
  output$groupText <- renderText({
    paste0(
       paste0("group1: ", find_group(
         c(input$store1, input$store2, input$store3), input$service, "1")),
       paste0("\ngroup2: ", find_group(
         c(input$store1, input$store2, input$store3), input$service, "2"))
     )
  })  
  
  output$profilePlot <- renderPlotly({
    print(
      ggplotly(
          agg_service_store_user_df  %>%
            filter(invoice_date >= as.Date(as.character(input$dateRange)[1]), 
                   invoice_date <= as.Date(as.character(input$dateRange)[2])) %>%
            filter(gender %in% c("男", "女")) %>%
            ggplot(., aes(age_group, fill = age_group)) +
            geom_bar() + 
            facet_wrap(gender~.)
      )
    )
  })  
  
  output$timeSeriesPlot <- renderPlotly({
    print(
      ggplotly(
        ggplot(data = generate_long_forecast_df(
          raw_long_df=total_long_store_service_df, 
          start_dt=as.character(input$dateRange)[1],
          end_dt=as.character(input$dateRange)[2],
          store=c(input$store1, input$store2, input$store3),
          service=input$service
        ), 
          aes(x=date, y=demand, colour=store, line_shape=algorithm)) + 
          geom_line() +
          facet_wrap(~service) + 
          ylim(0, 100)
        )
      )
  })
  
  output$demandTable <- renderDT(DT::datatable({
    data <- total_forecast_df 
    
    if (input$service != "All") {
      data <- data[data$service == input$service,]
    }
    # data <- data %>% 
    #   spread(algorithm, demand) # g1 & g2 has different column
    data
  }, filter = list(
    position = 'top', clear = FALSE
  ),
  options = list(
    pageLength = 15
  )), 
  )
  
}

