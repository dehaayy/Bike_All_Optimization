library(data.table)
library(sqldf)
options(scipen=999)


out_flow_path = "/Users/dehaay/Desktop/BikeShare Project/output/Start_station_count_all.csv"

in_flow_path = "/Users/dehaay/Desktop/BikeShare Project/output/End_station_count_all.csv"


########### GETS THE SIGNIFICANT STATIONS ########### 
year = 2022
number_of_significant_stations = 100

## OUTFLOW
out_datax = fread(out_flow_path)
out_dataz = out_datax[which(out_datax$year >= year),]
out_data = out_dataz[,-c(1:4)]

avg_hourly_st_out_flow <- apply(out_data,2,mean) #gets the avg of each column

avg_hrly_OUT_flow_df <- data.frame(names(avg_hourly_st_out_flow),avg_hourly_st_out_flow) #converts the result into df


## INFLOW

in_datax = fread(in_flow_path)
in_dataz = in_datax[which(in_datax$year >= year),]
in_data = in_dataz[,-c(1:4)]


avg_hourly_st_in_flow <- apply(in_data,2,mean)

avg_hrly_IN_flow_df <- data.frame(names(avg_hourly_st_in_flow),avg_hourly_st_in_flow)

colnames(avg_hrly_IN_flow_df) <- c("st_name","inflow")
colnames(avg_hrly_OUT_flow_df) <- c("st_name","outflow")

#Merges in and out dfs to find the the stations that contains in both and find the net flow
merged_df <- merge(avg_hrly_IN_flow_df, avg_hrly_OUT_flow_df, by = "st_name")

merged_df <- sqldf('SELECT st_name,SUM(inflow) as inflow,SUM(outflow) as outflow FROM merged_df GROUP BY st_name')



merged_df$netflow <- abs(merged_df$inflow - merged_df$outflow)
merged_df_sorted <- merged_df[order(-merged_df$netflow),]
#drop the NULL 

if (length(which(merged_df_sorted$st_name == "NULL")) > 0) {
  merged_df_sorted <- merged_df_sorted[-which(merged_df_sorted$st_name == "NULL"),]
}


#Get the top "number_of_significant_stations" stations' name
signif_station_names <- unique(merged_df_sorted[1:number_of_significant_stations,]$st_name)

fwrite(data.frame(signif_station_names),"/Users/dehaay/Desktop/BikeShare Project/output/sig_stations.csv")









