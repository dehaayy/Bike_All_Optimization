library(ranger)
library(jsonlite)
library(caret)
library(data.table)
library(tibble)
library(lubridate)


raw_data <- fread("/Users/dehaay/Desktop/BikeShare Project/output/Inflow_ML.csv")


raw_data <- add_column(raw_data, weekday = wday(raw_data$date, week_start = 1), .after = "day")

raw_data <- raw_data[,-c("rain_1h","rain_3h","snow_1h","snow_3h" )]
raw_data <- raw_data[,-c(7)]

#ADD WEEKDAY AND TRAIN BUT LOOK AT THEGAIN IT DID NOT MERGE CORRECTLY FOR THE LAST CHUNK DATA




## Remove weather columns that doesn't correspond to the API call

#### FOR TUNING ####
#raw_data <- raw_data[sample(1:nrow(raw_data),100),]
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

## Data Sampling ##
sampling_indexes <- createDataPartition(melted_data$hour_interval, p =0.80, list = FALSE)

train_data <- melted_data[sampling_indexes,]
test_data <- melted_data[-sampling_indexes,]

train_data <- train_data[,-c("date")]

test_data <- test_data[,-c("date")]



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









# # # # # # # # # KNN # # # # # # # # # # #
#Prediction time enourmous

knnmodel <- knnreg(train_data[,-12], as.numeric(unlist(train_data[,12])))
inputss <- data.frame(2013,
                      6,
                      1,
                      6,
                      2,
                      25.11,
                      25.48,
                      10000,
                      69,
                      1.03,
                      23)
colnames(inputss) <- colnames(test_data)[1:11]
#data.frame(test_data[,-12])
knn_pred <- predict(knnmodel, test_data[,-12])
# # # # # # # # # KNN # # # # # # # # # # #







# # # # # # GBM # # # # # # # # # # # # # #
library(gbm)
model_gbm <- gbm(value ~ ., 
                data = train_data, 
                distribution = "gaussian",
                cv.folds = 10,
                shrinkage = .01,
                n.minobsinnode = 10,
                n.trees = 500)

pred_y <- predict.gbm(model_gbm, test_data)
pred_y

test_y <- test_data$value
residuals = test_y - pred_y
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

y_test_mean = mean(test_y)
# Calculate total sum of squares
tss =  sum((test_y - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')
summary(model_gbm)
# # # # # # # # # # # # # ## # # # # # # # # # # # # ## # # # # # # # # # # # #








saveRDS(fit, "/Users/dehaay/Desktop/BikeShare Project/inflow_modell.rds")

live_current_weather <- as.data.frame(jsonlite::fromJSON("https://api.openweathermap.org/data/2.5/weather?lat=40.7143&lon=-74.006&appid=4a7c1c3a34bdf586deb2dc335fe4458d&units=metric"))


live_forcast_weather <- as.data.frame(jsonlite::fromJSON("http://api.openweathermap.org/data/2.5/forecast?id=5128581&appid=4a7c1c3a34bdf586deb2dc335fe4458d&units=metric"))