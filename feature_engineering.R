generate_long_forecast_df <- function(raw_long_df, start_dt, end_dt, store, service){
  # store <- c("056", "071") 1, 5, 18
  # service <- c("剪髮", "洗髮") 
  start_dt <- as.Date(start_dt)
  end_dt <- as.Date(end_dt)
  
  forecast_dt <- end_dt  %m+% months(1)
  forecast_h <- difftime(forecast_dt,  end_dt, 
                         units = c("days")) %>% as.numeric()
  if (service == "All"){
    service <- c("剪髮", "洗髮", "染髮", "燙髮", "護髮", "養護")
  }else{
    service <- service
  }
  store <- unique(store)
  store_service_cond <- apply(
    expand.grid(store, service), 1, paste, collapse="_")
  
  g1_service_cond <- store_service_cond[store_service_cond %in% g1_map]
  g2_service_cond <- store_service_cond[store_service_cond %in% g2_map]  
  
  raw_df <- raw_long_df %>% 
    filter(invoice_date >= start_dt, invoice_date <= forecast_dt) %>% 
    filter(store_service %in% store_service_cond)
  sub_df <- raw_df %>% filter(invoice_date <= end_dt)
  
  group_1_df <- sub_df %>% filter(group=="1") 
  group_2_df <- sub_df %>% filter(group=="2")
  
  if (nrow(group_1_df) > 0){
    g1_forecast_df_list <- list()
    for (idx in seq_along(g1_service_cond)){
      sub_store_service <- g1_service_cond[idx]
      ss_df <- group_1_df %>% filter(store_service == sub_store_service) # 102 row
      sub_raw <- raw_df %>% filter(store_service == sub_store_service) # some missing value
      
      new_raw <- data.frame(
        invoice_date=c(ss_df$invoice_date, seq(end_dt %m+% days(1), forecast_dt, by="days"))
      )
      new_raw <- merge(new_raw, sub_raw, all=TRUE)
      small_nn <- nnetar(ss_df$total)
      raw_forecast <- rollmean(ss_df$total, forecast_h) # 72 
      raw_forecast <- c(rep(NA, forecast_h), raw_forecast,
                        rep(raw_forecast[length(raw_forecast)], forecast_h-1)) # 102
      sub_fore_df <- data.frame(
        date=c(ss_df$invoice_date, seq(end_dt %m+% days(1), forecast_dt, by="days")),
        monthly_ma=raw_forecast,
        nn=c(small_nn$fitted %>% as.vector(), 
             as.data.frame(forecast(small_nn, h=forecast_h))$`Point Forecast`),
        raw=new_raw$total,
        store_service=sub_store_service
      )
      g1_forecast_df_list[[idx]] <- sub_fore_df
    }
    
    g1_forecast_df <- bind_rows(g1_forecast_df_list) %>%   
      select(date, store_service,raw, nn, monthly_ma) %>%
      tidyr::gather(., "algorithm", "demand", raw:monthly_ma, factor_key=TRUE)
  }else{
    g1_forecast_df<-data.frame(date=NA, store_service=NA, algorithm=NA, demand=NA)
  }
  
  if (nrow(group_2_df) > 0){
    g2_forecast_df_list <- list()
    for (idx in seq_along(g2_service_cond)){
      sub_store_service <- g2_service_cond[idx]
      ss_df <- group_2_df %>% filter(store_service == sub_store_service) # 102 row
      sub_raw <- raw_df %>% filter(store_service == sub_store_service) # some missing value
      
      new_raw <- data.frame(
        invoice_date=c(ss_df$invoice_date, seq(end_dt %m+% days(1), forecast_dt, by="days"))
      )
      new_raw <- merge(new_raw, sub_raw, all=TRUE)
      arima <- auto.arima(ss_df$total)
      raw_forecast <- rollmean(ss_df$total, 7)
      raw_forecast <- c(rep(NA, 7), raw_forecast, 
                        rep(raw_forecast[length(raw_forecast)], (forecast_h-1)))
      sub_fore_df <- data.frame(
        date=c(ss_df$invoice_date, seq(end_dt %m+% days(1), forecast_dt, by="days")),
        weekly_ma=raw_forecast,
        arima=c(arima$fitted %>% as.vector(), 
             as.data.frame(forecast(arima, h=forecast_h))$`Point Forecast`),
        raw=new_raw$total,
        store_service=sub_store_service
      )
      g2_forecast_df_list[[idx]] <- sub_fore_df
    }
    
    g2_forecast_df <- bind_rows(g2_forecast_df_list) %>%   
      select(date, store_service, raw, arima, weekly_ma) %>%
      tidyr::gather(., "algorithm", "demand", raw:weekly_ma, factor_key=TRUE)
  }else{
    g2_forecast_df<-data.frame(date=NA, store_service=NA, algorithm=NA, demand=NA)
  }
  total_forecast_df <- rbind(g1_forecast_df, g2_forecast_df) %>% as.data.frame()
  total_forecast_df <- total_forecast_df[1:(nrow(total_forecast_df)-1),]
  total_forecast_df["service"] <- total_forecast_df$store_service %>% 
    as.character() %>% 
    base::strsplit("_") %>% 
    lapply(function(x)x[[2]]) %>% 
    unlist()
  total_forecast_df["store"] <- total_forecast_df$store_service %>% 
    as.character() %>% 
    base::strsplit("_") %>% 
    lapply(function(x)x[[1]]) %>% 
    unlist()
  total_forecast_df <<- total_forecast_df
  return(total_forecast_df)
}


