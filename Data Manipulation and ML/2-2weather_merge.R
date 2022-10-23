library(data.table)
library(dplyr)
library(lubridate)
library(sqldf)

out_flow_path = "/Users/dehaay/Desktop/BikeShare Project/output/Start_station_count_all.csv"

in_flow_path = "/Users/dehaay/Desktop/BikeShare Project/output/raw_End_station_count_all.csv"

signf_station_path = "/Users/dehaay/Desktop/BikeShare Project/output/sig_stations.csv"

weather_path <- "/Users/dehaay/Desktop/BikeShare Project/raw_data/2a0bb0adc7ef7b8192add6f753a89448.csv"

out_datax = fread(out_flow_path)
in_datax = fread(in_flow_path)
sig_stations = fread(signf_station_path,sep="\n")

##### MANUEL ADJUSTMENT FOR "Riverside Dr & W 72 St" ####

out_rep <- which(colnames(out_datax) == "Riverside Dr & W 72 St")
out_comb <- select(out_datax,out_rep[1]) + select(out_datax,out_rep[2])
out_datax <- out_datax[,-c("Riverside Dr & W 72 St")]
out_datax$`Riverside Dr & W 72 St` <- out_comb

in_rep <- which(colnames(in_datax) == "Riverside Dr & W 72 St")
in_comb <- select(in_datax,in_rep[1]) + select(in_datax,in_rep[2])
in_datax <- in_datax[,-c("Riverside Dr & W 72 St")]
in_datax$`Riverside Dr & W 72 St` <- in_comb
### # # # # # # # # # # # # # # # # # # # # # # # # # # # # ###


valid_OUTflow_column_index <- which(colnames(out_datax) %in% sig_stations$signif_station_names)

valid_INflow_column_index <- which(colnames(in_datax) %in% sig_stations$signif_station_names)


out_data <- select( out_datax, c("year","month","day","hour_interval",valid_OUTflow_column_index) )

in_data <- select( in_datax, c("year","month","day","hour_interval",valid_INflow_column_index) )



##### DATE GATHERING ####
date <- make_datetime(
  year = out_data$year,
  month = out_data$month,
  day = out_data$day,
  hour = out_data$hour_interval,
  tz = "America/New_York"
)

out_data<- cbind(date,out_data)



date <- make_datetime(
  year = in_data$year,
  month = in_data$month,
  day = in_data$day,
  hour = in_data$hour_interval,
  tz = "America/New_York"
)

in_data<- cbind(date,in_data)



###### WEATHER DATA PROCESSING ###### 

weather_raw <- fread(weather_path)

## Weather data formatting

weather_date <- substr(weather_raw$dt_iso,1,19)
weather_date <- as.POSIXct(weather_date, format= "%Y-%m-%d %H:%M:%S")
weather_date <-force_tz(weather_date, tzone = "UTC")
weather_date <-with_tz(weather_date, tzone = "America/New_York")
weather_raw$dt_iso <- weather_date

weather_final <- data.frame(date = weather_raw$dt_iso, temp = weather_raw$temp,
                          feels_like = weather_raw$feels_like, visblty = weather_raw$visibility,
                          humidity = weather_raw$humidity, windspd = weather_raw$wind_speed , 
                          rain_1h = weather_raw$rain_1h, rain_3h=	weather_raw$rain_3h,
                          snow_1h=weather_raw$snow_1h, snow_3h=weather_raw$snow_3h,weather_main = weather_raw$weather_main)

weather_final <- weather_final[-which(duplicated(weather_final$date)),]
###### WEATHER AND BIKE MERGING  ###### 


part <- out_data[,6:ncol(out_data)]
out_data <- out_data[rowSums(part[]) > 0 , ] #removes all duplicate rows (rows that bike part sum up to zero)




## inner join
OUT_merged_df <- sqldf("SELECT *
              FROM out_data
              JOIN weather_final ON out_data.date = weather_final.date")


weather_portion_of_data <- OUT_merged_df[,c(106:115)]
rrr <- cbind(OUT_merged_df[,1:5],weather_portion_of_data) #FIRST PART OF CBIND
OUT_merged_df <- cbind(rrr,OUT_merged_df[,6:105])

  
  
#for input delete all zero rows 
part <- in_data[,6:ncol(in_data)]
in_data <- in_data[rowSums(part[]) > 0 , ] #removes all duplicate rows (rows that bike part sum up to zero)

IN_merged_df <- sqldf("SELECT *
              FROM in_data
              JOIN weather_final ON in_data.date = weather_final.date")

weather_portion_of_data <- IN_merged_df[,c(106:115)]
rrr <- cbind(IN_merged_df[,1:5],weather_portion_of_data) #FIRST PART OF CBIND
IN_merged_df <- cbind(rrr,IN_merged_df[,6:105])


View(OUT_merged_df)
View(IN_merged_df)



fwrite(OUT_merged_df,"/Users/dehaay/Desktop/BikeShare Project/output/Outflow_ML.csv")
fwrite(IN_merged_df,"/Users/dehaay/Desktop/BikeShare Project/output/Inflow_ML.csv")
