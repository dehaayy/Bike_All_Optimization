library(data.table)

data <- fread("/Users/dehaay/Desktop/BikeShare Project/experiment/allocation_mock_data.csv")

data$`capacity %` <- (data$`bike count in an hour` / data$`station capacity`)


#### INTRO ####  
    #On the real application make sure to check that the difference between max and min station can be very big, and there might be very small stations a capacity <15 shouldnt be accaptable
equilibrium_percentage <- (sum(data$`bike count in an hour`) / sum(data$`station capacity`)) # the most optimal capacity % when all bikes are allocated
allowed_min_bike_count = 1 #The number of bike the smallest station can have after optmization
#Allows the smallest station to have +- 1 bike from the optimal, we use the min st. capacity since it has the largest variance and make sures that every station can have ATLEAST "allowed_min_bike_count" many bikes remaining to optimize [gives flexibility to optimization and avoids very long runtime]
allowed_equilibrium_upperb <- ceiling((equilibrium_percentage + (allowed_min_bike_count/min(data$`station capacity`))) * 100) / 100
allowed_equilibrium_lowerb <- floor((equilibrium_percentage - (allowed_min_bike_count/min(data$`station capacity`))) * 100) / 100

max_possible_leftover = (allowed_min_bike_count/min(data$`station capacity`)) / (allowed_min_bike_count/max(data$`station capacity`)) # +- bike leftover from the optimized station capacity

  ### number of bikes for the optimal "equilibrium_percentage" --> Capacity * equilibrium_percentage. 
  ### [Uses as a start point, allowed upper and lower bounds will be used a conditions later in the while loop to exit the optimization] ###
data$optimal_bike_stock <- round((data$`station capacity` * equilibrium_percentage ))

data$bike_allocation_needed <- data$optimal_bike_stock - data$`bike count in an hour`





#### FUNCTIONS ####
  #### This function computes the distances between all stations and the parameter station name
        #Based on the number of bikes needed to be allocated, filters for only the stations that the allocation can be done (if need is postive num. of bikes, displays only the negative num. of bikes (overflow) )
        #Sorts the stations respect to their distance to the parameter station
get_allocation_table <- function(name) {
 pivot_station <- data[which(data$`Station names` == name),] #Station that we are calculating in comparison
 
 overflow_table = TRUE ## By default assume the "bike_allocation_needed" is >0 so the station need bikes, thus only "-" (overflow) bikes are added to the table
 if (pivot_station$bike_allocation_needed < 0) {
   overflow_table = FALSE 
 }
 
 distance_table <- data[-c(which(data$`Station names` == name)),]
 distance_table$distance <- abs(pivot_station$`x cord`-distance_table$`x cord`) + abs(pivot_station$`y cord`-distance_table$`y cord`)
 
 if (overflow_table) {
   distance_table <- distance_table[which(distance_table$bike_allocation_needed < 0),]
 } else {
   distance_table <- distance_table[which(distance_table$bike_allocation_needed > 0),]
 }
 distance_table <- distance_table[order(distance_table$distance),]
 
 return (distance_table)
}

# Piece of "get_process_order" gets the name of the stations that are outside the allowed range, the passed in data is ordered 
get_applicable_stations <- function(capacity,Station_names) {
  if ( (capacity < allowed_equilibrium_lowerb) || (capacity > allowed_equilibrium_upperb) ){
    return(Station_names)
  }
}


#### Gets the order of station names to process, in the order of priority. The first element is the first station to be solved, while the last on the list is the least needs of optimization
get_process_order <- function(the_data) {
  mock_data <- the_data
  mock_data$bike_allocation_needed <- abs(mock_data$bike_allocation_needed) # get the absolute value to disregard the over and underflow + and - problem
  mock_data <- mock_data[order(-mock_data$bike_allocation_needed),]
  
  
  return (unlist(mapply(get_applicable_stations,mock_data$`capacity %`,mock_data$`Station names`)))
}





# update_data("B","CGHU",14) read as: allocate 14 bikes to B -> CGHU
# update_data("B","CGHU",-14) would be read as: allocate 14 bikes to CGHU - > B 
update_data <- function(pivot_station_name, iteration_station_name, num_transfer_bike) {
  
  t_data <- data #temp data
  
  pivot_row_location <- which(t_data$`Station names` == pivot_station_name)
  #Update the pivot row
  t_data[pivot_row_location,]$`bike count in an hour` <- t_data[pivot_row_location,]$`bike count in an hour` - num_transfer_bike
  #Recalculate other percentages
  t_data[pivot_row_location,]$`capacity %` <- t_data[pivot_row_location,]$`bike count in an hour` / t_data[pivot_row_location,]$`station capacity`
  t_data[pivot_row_location,]$`bike_allocation_needed` <- t_data[pivot_row_location,]$`bike_allocation_needed` + num_transfer_bike
  
  #Update the iteration station table
  iter_row_location <- which(t_data$`Station names` == iteration_station_name)
  
  t_data[iter_row_location,]$`bike count in an hour` <- t_data[iter_row_location,]$`bike count in an hour` + num_transfer_bike
  t_data[iter_row_location,]$`capacity %` <- t_data[iter_row_location,]$`bike count in an hour` / t_data[iter_row_location,]$`station capacity`
  t_data[iter_row_location,]$`bike_allocation_needed` <- t_data[iter_row_location,]$`bike_allocation_needed` - num_transfer_bike
  
  return(t_data)
}


if_na <- function (input) { #Handles an error for the case if the allocation table can not be generated because the table is balanced earlier {
  if (is.na(input)) {
    return (0)
  } else {
    return(input)
  }
}

#Creates the report table empty first row gives the initial standard deviation 
report_table <- data.frame(pivot_station = c("-"), itter_station = c("-"), bike_allocation_count = 0 , dist = 0 , 'STDEV' = sd(data$`capacity %`) * 100 , Generation = c(0), Iteration = 0, weight_per = 0)


############################################

data_backup <- data # delete later this is for debugging






#### ITTERATION ####
generation = 0
iteration_count = 0

#
the_order <- get_process_order(data)

#the_order <- sample(the_order)

for (pivot_st_name in the_order) {
  allocation_table <- get_allocation_table(pivot_st_name) #the allocation table for the pivot station (the station the algorithm is currently working on)
  pivot_station_row <- data[which(data$`Station names` == pivot_st_name),] #this is the row of the current station that allocation table is built on
  generation = generation + 1
  
  #In further iteration if the station needs are already filled by previous allocations, the station is skipped
  if (data[which(data$`Station names` == pivot_st_name),]$bike_allocation_needed == 0){
    next
  }
  
  
  # Loops through the allocation table for the given pivot station
  for (allc_table_rows in 1:nrow(allocation_table)) {
    iteration_count = iteration_count + 1
    #print(allocation_table[allc_table_rows,])
    
    t_bike_all_needed <- allocation_table[allc_table_rows,]$bike_allocation_needed #allocation table's corresponding row's  # of bike_allocation_needed
    t_station_name <- allocation_table[allc_table_rows,]$`Station names` #allocation table's corresponding row's station name
    t_dist <- allocation_table[allc_table_rows,]$distance #allocation table's corresponding row's distance
    
    #Decides the number of bike needed to transfer, computes which number is bigger : 
                          #Pivot station's need or the allocation stations need (Ability to give or get). 
                          #It filter if our pivot station needs 5 bikes but the allocation station can provide 20 bikes, allows us to decide the transfer as just 5. Or if our pivot need 15 and allocation can give only 5 then decides 10 as the transfer bike
   

    if ( if_na(abs(pivot_station_row$bike_allocation_needed) - abs(t_bike_all_needed) ) > 0   ) {
      num_transfer_bikes = t_bike_all_needed
    } else {
      num_transfer_bikes = (pivot_station_row$bike_allocation_needed) * -1 # *-1 is just works for the mathematical model I came up with to manage give/take at a single condition. 
    }
    
    #Update the table with the allocation
    data <- update_data(pivot_st_name,t_station_name,num_transfer_bikes)
    pivot_station_row <- data[which(data$`Station names` == pivot_st_name),]
    #Update the Report
    new_report_row <- data.frame(pivot_station = pivot_st_name, itter_station = t_station_name, bike_allocation_count = num_transfer_bikes, dist = t_dist,  'STDEV' = sd(data$`capacity %`) * 100, Generation = c(generation), Iteration = iteration_count , weight_per = (sum(abs(report_table$bike_allocation_count) * report_table$dist) ) / nrow(report_table) )
    print(nrow(report_table))
    report_table <- rbind(report_table,new_report_row)
    # Quitting constraints
    if( (allc_table_rows == nrow(allocation_table)) || (pivot_station_row$bike_allocation_needed == 0)) {
      break
    }
    
  }
  iteration_count = 0
  

}

report_table <- na.omit(report_table)
total_weight = (sum(abs(report_table$bike_allocation_count) * report_table$dist) )
print(total_weight)


View(report_table)
plot(report_table$STDEV, type = "l", lty = 1)






#TRY 
#Randomizing the "get_process_order" and see if that imporves the combined score




# In the document at every stage take note of the itteration, itteration within itteration, and standart deviation 

