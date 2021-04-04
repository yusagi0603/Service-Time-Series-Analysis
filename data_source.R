save_dir <- "/Users/yuu/Project/happy_hair_db/"

# read in data
agg_service_store_user_df <- readr::read_csv(
  paste0(save_dir,"agg_service_store_user_df.csv")
)
agg_service_store_user_df["store_service"] <- paste0(
  agg_service_store_user_df$store_code, "_", agg_service_store_user_df$category_name
)
total_long_store_service_df <- agg_service_store_user_df %>% 
  dplyr::group_by(invoice_date, store_service) %>%
  dplyr::summarise(total=sum(total))

store_service_map <- readr::read_csv(paste0(save_dir,"store_service_map_group.csv"))[,c(1, 2)]
g1_map <- (store_service_map %>% filter(group=="1"))$store_service
g2_map <- (store_service_map %>% filter(group=="2"))$store_service

total_long_store_service_df <- merge(
  x=total_long_store_service_df, 
  y=store_service_map, 
  by="store_service", all = TRUE) # take 5 seconds
total_long_store_service_df["store"] <- total_long_store_service_df$store_service %>% 
  as.character() %>% strsplit("_") %>% sapply(function(x)x[[1]])
total_long_store_service_df["service"] <- total_long_store_service_df$store_service %>% 
  as.character() %>% strsplit("_") %>% sapply(function(x)x[[2]])
total_long_store_service_df["invoice_date"] <- total_long_store_service_df$invoice_date %>% as.Date()

total_long_store_service_df <- total_long_store_service_df %>% 
  arrange(invoice_date)

all_unique_store <- total_long_store_service_df$store %>% unique()
all_unique_service <- total_long_store_service_df$service %>% unique()

find_group <- function(input_store, input_service, group_number){
  if (input_service == "All"){
    input_service <- c("剪髮", "洗髮", "染髮", "燙髮", "護髮", "養護")
  }else{
    input_service <- c(input_service)
  }  
  
  # input_service: a vector
  target_store_service <- data.frame(store_service=apply(
    expand.grid(input_store, input_service), 1, paste, collapse="_")
    )
  sub_map <- store_service_map %>% filter(group==group_number)
  join_res <- merge(target_store_service, sub_map)$store_service
  join_res <- paste(join_res, collapse=" / ")
  return(join_res) 
}



