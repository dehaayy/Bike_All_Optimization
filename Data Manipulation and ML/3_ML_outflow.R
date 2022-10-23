library(ranger)
library(jsonlite)
library(caret)
library(data.table)
library(tibble)
library(lubridate)


raw_data <- fread("/Users/dehaay/Desktop/BikeShare Project/output/Outflow_ML.csv")


raw_data <- add_column(raw_data, weekday = wday(raw_data$date, week_start = 1), .after = "day")

raw_data <- raw_data[,-c("rain_1h","rain_3h","snow_1h","snow_3h" )]
raw_data <- raw_data[,-c(7)]

#ADD WEEKDAY AND TRAIN BUT LOOK AT THEGAIN IT DID NOT MERGE CORRECTLY FOR THE LAST CHUNK DATA




## Remove weather columns that doesn't correspond to the API call

#### FOR TUNING ####
#raw_data <- raw_data[sample(1:nrow(raw_data),6000),]
# # # # # # # # # #




melted_data <-  melt(raw_data, id = c(colnames(raw_data[,1:11])) )

## Factorize Station 
station_factors <- unclass(factor(melted_data$variable))
station_dictionary <- data.frame(factors  = c(1 : length(levels(station_factors))), station_names =  levels(station_factors) )
melted_data$variable <- station_factors

## Factorize Weather 
#weather_descp_factors <- unclass(factor(melted_data$weather_main))
#weather_dictionary <- data.frame(factors  = c(1 : length(levels(weather_descp_factors))), weather_names =  levels(weather_descp_factors) )
#melted_data$weather_main <- weather_descp_factors

# Imputes the na values
melted_data <- setnafill(melted_data, type = "locf")



## To have a more accurate training mix the data, so it sees all stations for every date
melted_data <-melted_data[order(date,variable),]


#melted_data <- melted_data[,-c("date")]
fwrite(melted_data, "/Users/dehaay/Desktop/BikeShare Project/train and test/melted_data.csv")



## Data Sampling ##
sampling_indexes <- createDataPartition(melted_data$hour_interval, p =0.80, list = FALSE)

train_data <- melted_data[sampling_indexes,]
test_data <- melted_data[-sampling_indexes,]

train_data <- train_data[,-c("date")]

test_data <- test_data[,-c("date")]

View(train_data)



##### RANGER ######
fit <- ranger(value ~ ., 
              data = train_data, 
              num.trees = 140,
              max.depth = 55,mtry = 11,importance='impurity')

fit

ranger::importance(fit) / sum(ranger::importance(fit)) * 100


ranger_predx <- predict(fit, test_data)$predictions

df_pred <- data.frame(prediction = ranger_predx , actual = test_data$value)

dex <- sample(1:nrow(test_data),54000)

plot(ranger_predx[dex],test_data$value[dex])



saveRDS(fit, "/Users/dehaay/Desktop/BikeShare Project/outflow_model.rds")
