#install.packages("readxl")
#install.packages("lubridate")
#Install all the packages
library(readxl)
library(lubridate)
library(plyr)
library(fasttime)
library(data.table)
library(dplyr)


timer = Sys.time()

### Function to convert date formats to a standart format
use_ymd_hms <- function(x){
  
  tryCatch(  
    expr = {
      year <- as.numeric(substr(x,1,4))
      return (TRUE)
    },
    error = function(e){
      message('Caught an error!')
      print(e)
    },
    warning = function(w){
      #message('Caught an warning!')
      message('All done, date format fixed.')
      print(w)
      return (FALSE)
      print("m/d/y format")
    },
    finally = {
      
    }
    
  )
  
}
###

#Goes to the selected folder
path_to_dir_folder <- "/Users/dehaay/Desktop/BikeShare Project/raw_data/test"

#Gets and stores all the paths from that folder in the "sub_paths" array.
sub_paths <- list.files(path=path_to_dir_folder, pattern=NULL, all.files=FALSE, full.names=TRUE)
sub_paths

#Sets and empty data frame for merging
previous <- data.frame()




for( i in sub_paths) {
  print(i)
  
  
  
  data_being_read <- na.omit(fread(i))
  

  
  
  ########Corrects the old formatted data########
  #Based on the keywords within the csv folders being scanned,
  #the version of the data is identified, neccesary columns are seperated
  #and the date format is commonized (ymd - mdy and all in NYtime)
  
  if(colnames(data_being_read)[1] == "tripduration") {
    print("OLD FILE")
    
    data_being_read <- data.frame(as.character(data_being_read$starttime), as.character(data_being_read$stoptime) ,data_being_read$`start station name`,
                                  data_being_read$`end station name`)
    
    colnames(data_being_read) <- c("started_at"	,
                                   "ended_at",	"start_station_name",	
                                   "end_station_name")
    
    if (use_ymd_hms( as.character(data_being_read$started_at)[1] )) {
      print(use_ymd_hms( as.character(data_being_read$started_at)[1] ))
      data_being_read$started_at <- ymd_hms(data_being_read$started_at ,truncated = 1,tz = "America/New_York")
      data_being_read$ended_at <- ymd_hms(data_being_read$ended_at ,truncated = 1,tz = "America/New_York")
    } else {
      print(use_ymd_hms( as.character(data_being_read$started_at)[1] ))
      data_being_read$started_at <- mdy_hms(data_being_read$started_at ,truncated = 1,tz = "America/New_York")
      data_being_read$ended_at <- mdy_hms(data_being_read$ended_at ,truncated = 1,tz = "America/New_York")
    }
    
    
  } else if (colnames(data_being_read)[1] == "Trip Duration") {
    print("MID OLD FILE")
    
    
    
    data_being_read <- data.frame(as.character(data_being_read$`Start Time`),
                                  as.character(data_being_read$`Stop Time`), data_being_read$`Start Station Name`,
                                  data_being_read$`End Station Name`)
    
    
    colnames(data_being_read) <- c("started_at"	,
                                   "ended_at",	"start_station_name",	
                                   "end_station_name")
    
    if (use_ymd_hms( as.character(data_being_read$started_at)[1] )) {
      print(use_ymd_hms( as.character(data_being_read$started_at)[1] ))
      data_being_read$started_at <- ymd_hms(data_being_read$started_at ,truncated = 1,tz = "America/New_York")
      data_being_read$ended_at <- ymd_hms(data_being_read$ended_at ,truncated = 1,tz = "America/New_York")
    } else {
      print(use_ymd_hms( as.character(data_being_read$started_at)[1] ))
      data_being_read$started_at <- mdy_hms(data_being_read$started_at ,truncated = 1,tz = "America/New_York")
      data_being_read$ended_at <- mdy_hms(data_being_read$ended_at ,truncated = 1,tz = "America/New_York")
    }
    
    
  } else {
    print("NEW FORMAT FILE")
    data_being_read <- data.frame(as.character(data_being_read$`started_at`),
                                  as.character(data_being_read$`ended_at`), data_being_read$`start_station_name`,
                                  data_being_read$`end_station_name`)
    colnames(data_being_read) <- c("started_at"	,
                                   "ended_at",	"start_station_name",	
                                   "end_station_name")
    
    if (use_ymd_hms( as.character(data_being_read$started_at)[1] )) {
      
      data_being_read$started_at <- ymd_hms(data_being_read$started_at , truncated = 1,tz = "America/New_York")
      data_being_read$ended_at <- ymd_hms(data_being_read$ended_at ,truncated = 1,tz = "America/New_York")
    } else {
      data_being_read$started_at <- mdy_hms(data_being_read$started_at , truncated = 1,tz = "America/New_York")
      data_being_read$ended_at <- mdy_hms(data_being_read$ended_at ,truncated = 1,tz = "America/New_York")
    }
    
  }
  ################################################ 
  
  #binds each of the dataframes to a single dataframe
  data <- rbind(previous, na.omit(data_being_read) )
  
  previous <- data
  
}



library(tidyverse)
#### Start Station Work --Produces: start_station_count  ####

start_station_data <- data.frame(year = lubridate::year(data$started_at))
start_station_data$month <- lubridate::month(data$started_at)
start_station_data$day <- lubridate::day(data$started_at)
start_station_data$hour_interval <- lubridate::hour(data$started_at)
start_station_data$start_station_name <- data$start_station_name

#Check for no station names
if ( sum(which(start_station_data$start_station_name == "")) != 0 ) {
  start_station_data <- start_station_data[-c(which(start_station_data$start_station_name == "")),]
}

## Creates the count table of every transaction, outputs in the data format "tibble"
start_station_data_tibble <- start_station_data %>%
  count(year, month, day, hour_interval, start_station_name) %>%
  pivot_wider(names_from = start_station_name, values_from = n, values_fill = 0)

start_station_count<- data.frame(start_station_data_tibble)
colnames(start_station_count) <- colnames(start_station_data_tibble) 




print("LINE 157")
end_station_data <- data.frame(year = lubridate::year(data$ended_at))
end_station_data$month <- lubridate::month(data$ended_at)
end_station_data$day <- lubridate::day(data$ended_at)
end_station_data$hour_interval <- lubridate::hour(data$ended_at)
end_station_data$end_station_names <- data$end_station_name

if ( sum(which(end_station_data$end_station_names == "")) != 0 ) {
  end_station_data <- end_station_data[-c(which(end_station_data$end_station_names == "")),]
}
  


## Creates the count table of every transaction, outputs in the data format "tibble"
print("LINE 165")
end_station_data_tibble <- end_station_data %>%
  count(year, month, day, hour_interval, end_station_names) %>%
  pivot_wider(names_from = end_station_names, values_from = n, values_fill = 0)


#count year i df ye aktatip null value icin look up et 
print("LINE 170")
end_station_count<- data.frame(end_station_data_tibble)
colnames(end_station_count) <- colnames(end_station_data_tibble)




