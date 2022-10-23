allowed_range_coeff <- 0.1
#As methodology takes the distance to the bounds
#allowes the data to be within the distance to the (bounds * allowed_range_percent) away from the current value
#Using this it never allows a station to be less or more than the either bound

equilibrium_percentage <- (sum(data$`bike count in an hour`) / sum(data$`station capacity`)) # the most optimal capacity % when all bikes are allocated
allowed_equilibrium_upperb <- ((1 - equilibrium_percentage) * (allowed_range_coeff) ) + equilibrium_percentage # allowed max upper % (+0.1 is there, because since stations are usually will be more tend to fuller capcity, it allows and added flexibility )
allowed_equilibrium_lowerb <- equilibrium_percentage - ((equilibrium_percentage) * allowed_range_coeff ) # allowed max lower %


# Piece of "get_process_order" gets the name of the stations that are outside the allowed range, the passed in data is ordered 
get_applicable_stations <- function(capacity,Station_names) {
 if ( (capacity < allowed_equilibrium_lowerb) || (capacity > allowed_equilibrium_upperb) ){
   return(Station_names)
 }
}

unlist(mapply(checkk,data$`capacity %`,data$`Station names`))













report_table <- data.frame(pivot_station = c("-"), itter_station = c("-"), bike_allocation_count = 0 , Generation = c(0), Iteration = 0, 'STDEV' = sd(data$`capacity %`) * 100 )

#### TESTING OF THE ITTERATION THIS IS ONLY FOR A SINGLE GIVEN CASE 

pivot_st_name = "I" 

allocation_table <- get_allocation_table(pivot_st_name) #the allocation table for the 
pivot_station_row <- data[which(data$`Station names` == pivot_st_name),] #this is the row of the current station that allocation table is built on
generation = 1
iteration_count = 0

# Loops through the allocation table for the given pivot station
for (allc_table_rows in 1:nrow(allocation_table)) {
  iteration_count = iteration_count + 1
  #print(allocation_table[allc_table_rows,])
  
  t_bike_all_needed <- allocation_table[allc_table_rows,]$bike_allocation_needed #allocation table's corresponding row of # of bike_allocation_needed
  t_station_name <- allocation_table[allc_table_rows,]$`Station names`
  
  if( ( abs(pivot_station_row$bike_allocation_needed) - abs(t_bike_all_needed) ) > 0   ){
    num_transfer_bikes = t_bike_all_needed
  } else {
    num_transfer_bikes = (pivot_station_row$bike_allocation_needed) * -1
  }
  
  #Update the table with the allocation
  data <- update_data(pivot_st_name,t_station_name,num_transfer_bikes)
  pivot_station_row <- data[which(data$`Station names` == pivot_st_name),]
  #Update the Report
  new_report_row <- data.frame(pivot_station = pivot_st_name, itter_station = t_station_name, bike_allocation_count = num_transfer_bikes , Generation = c(generation), Iteration = iteration_count, 'STDEV' = sd(data$`capacity %`) * 100 )
  report_table <- rbind(report_table,new_report_row)
  # Quitting constraints
  if( (allc_table_rows == nrow(allocation_table)) || (pivot_station_row$bike_allocation_needed == 0)) {
    break
  }
}
iteration_count = 0

