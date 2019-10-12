rm (list = ls()) # Cleaing the evironment
setwd("C:/Users/Sridhar/Desktop/project on prediction/prediction on cab/3") #setting directory
getwd()

#Reading the data
  cabtest = read.csv('test.csv')
  cabfare = read.csv('train_cab.csv')
  
  #knowing structure of our data
  str(cabfare) #We have 16067 obs. of  7 variables
  sum(is.na(cabfare))
  
  ## EDA
  #Changing the data types of our variables
  cabfare$fare_amount = as.numeric(as.character(cabfare$fare_amount))
  cabfare$passenger_count = as.integer(as.character(cabfare$passenger_count))
  cabfare$pickup_datetime = gsub( " UTC", "", as.character(cabfare$pickup_datetime))
  
  summary(cabfare)
  
  ##DATA PRE-POCESSING
  
  #1 Data Cleaning
  cabfare$passenger_count[cabfare$passenger_count > 8] = 2 
  cabfare$pickup_latitude[cabfare$pickup_latitude > 90 ] = 0
  cabfare$fare_amount[cabfare$fare_amount > 100 ] = 0
  cabfare$fare_amount[cabfare$fare_amount < 0 ] = 0
  
  #Extracting the date time year month from date column 
  library(dplyr) #for using mutate
  library(lubridate) #for using ymd_hms
  cabfare = mutate(cabfare,
                   pickup_datetime = ymd_hms(`pickup_datetime`),
                   month = as.integer(month(pickup_datetime)),
                   year = as.integer(year(pickup_datetime)),
                   dayOfWeek = as.integer(wday(pickup_datetime)),
                   hour = hour(pickup_datetime),
                   hour = as.integer(hour(pickup_datetime))
  )
  
  #replacing all the 0's in Data as NA
  cabfare[cabfare == 0] = NA
  
  ##Changing -ve values to positive by usning abs function
  cabfare$pickup_longitude = abs(cabfare$pickup_longitude)
  cabfare$pickup_latitude = abs(cabfare$pickup_latitude)
  cabfare$dropoff_longitude = abs(cabfare$dropoff_longitude)
  cabfare$dropoff_latitude = abs(cabfare$dropoff_latitude)
  
  #for caluclating distance from logi/lati
  library(geosphere)
  cabfare = cabfare %>% 
    mutate(distance = by(cabfare, 1:nrow(cabfare), function(row) { 
      distHaversine(c(row$pickup_longitude, row$pickup_latitude), c(row$dropoff_longitude,row$dropoff_latitude))/1000}))
  summary(cabfare$distance)
  
  
  cabfare$distance[cabfare$distance > 500 ] = 0
  cabfare$distance[cabfare$distance < 0 ] = 0
  
  
  #2.Missing value analysis 
  Missing_val = data.frame(sapply(cabfare, function(x) sum(is.na(x))))
  
  #Show Percentage of missing value
  Missing_val$Variables = row.names(Missing_val)
  row.names(Missing_val) = NULL
  names(Missing_val)[1] = "Missing_values"
  Missing_val$Missing_percentage=(Missing_val$Missing_values/nrow(cabfare))*100
  Missing_val
  
  ##Replacing missing values with median in R
  cabfare$fare_amount[is.na(cabfare$fare_amount)] = median(cabfare$fare_amount, na.rm = T)
  cabfare$pickup_datetime[is.na(cabfare$pickup_datetime)] = median(cabfare$pickup_datetime, na.rm = T)
  cabfare$pickup_longitude[is.na(cabfare$pickup_longitude)] = median(cabfare$pickup_longitude, na.rm = T)
  cabfare$pickup_latitude[is.na(cabfare$pickup_latitude)] = median(cabfare$pickup_latitude, na.rm = T)
  cabfare$dropoff_longitude[is.na(cabfare$dropoff_longitude)] = median(cabfare$dropoff_longitude, na.rm = T)
  cabfare$dropoff_latitude[is.na(cabfare$dropoff_latitude)] = median(cabfare$dropoff_latitude, na.rm = T)
  cabfare$passenger_count[is.na(cabfare$passenger_count)] = median(cabfare$passenger_count, na.rm = T)
  cabfare$hour[is.na(cabfare$hour)] = median(cabfare$hour, na.rm = T)
  cabfare$year[is.na(cabfare$year)] = median(cabfare$year, na.rm = T)
  cabfare$month[is.na(cabfare$month)] = median(cabfare$month, na.rm = T)
  cabfare$dayOfWeek[is.na(cabfare$dayOfWeek)] = median(cabfare$dayOfWeek, na.rm = T)
  cabfare$distance[is.na(cabfare$distance)] = median(cabfare$distance, na.rm = T)
  
  sum(is.na(cabfare))
  
  
  #3.Visitualisation 
  library(ggplot2)
  library(scales)
  
  #plotting only some Continuous variables vs target variable 
  #PLoting the graph for hour and Fare
  ggplot(cabfare,aes(x = hour, y = fare_amount))+
    geom_line()+
    labs(x= "hour of the day")+
    scale_x_discrete(limits = c(0:23))+
    scale_y_continuous(limits=c(0,180))
  #From the above graph we can see that the timeing is not affecting too much. Maximin dots are below 100.
  
  #PLoting the graph for passanger_count and Fare
  gplot_p = ggplot(data=cabfare, aes(x=passenger_count, y=fare_amount)) + geom_point()+ geom_line()+ 
    ggtitle("Time and Fare Plot") +
    xlab("Passenger Count ") + 
    ylab("Fare")
  gplot_p
  # From the Graph it seems passenger count is not affecting the fare and frequency of 1 pssenges are high
  
  #PLoting the graph for distance and Fare
  gplot = ggplot(data=cabfare, aes(x=cabfare$distance, y=cabfare$fare_amount)) + geom_point()+ geom_line()+ 
    ggtitle("Distance and Fare Plot") +
    xlab("Distance in KM ") + 
    ylab("Fare")
  gplot
  

  #4.Outlier Analysis 
  numeric_index = sapply(cabfare, is.numeric) # creating numerical value index
  numeric_data = cabfare[,numeric_index] # storing numeric data
  cnames = colnames(numeric_data) #storing numeric data column names
  summary(numeric_data)
  
  #Creating box-plot to analyze outliers
  for (i in 1:length(cnames)){
    assign(paste0("gn", i), ggplot(aes_string(y = cnames[i], x = "fare_amount"), data = subset(cabfare)) +
             stat_boxplot(geom = "errorbar", width = 0.5) + 
             geom_boxplot(outlier.colour = "red", fill = "blue", outlier.shape = 18, outlier.size = 1, notch = FALSE) + 
             theme(legend.position = "bottom") + labs(y = cnames[i], x="fare") + ggtitle(paste("Boxplot of fare for", cnames[i])))
  }
  gridExtra::grid.arrange(gn2, gn3, gn4,gn5, gn6, gn7,gn8, gn9, gn10, gn11, ncol = 4, nrow = 3) # exclud if gn1 as that is unique for each observation
  
  #replace outliers with NA 
  for(i in cnames) {
    print(i)
    val = cabfare[,i][cabfare[,i] %in% boxplot.stats(cabfare[,i]) $out]
    
    print(length(val))
    cabfare[,i][cabfare[,i] %in% val] = NA
  }
  
  #imputing NA values--deleting
  cabfare=na.omit(cabfare)
  
  summary(cabfare)
  
  
  #5 Feature Selection
  #check skwness of the target variable
  library(PerformanceAnalytics)
  skew_xts = skewness(cabfare$fare_amount)
  skew_xts
  #1.0
  
  
  #6.Feature selection 
  #install.packages("corrgram")
  library(corrgram)
  
  #Correlation check on continuous variable
  round(cor(numeric_data),2) #Correlation table column wise
  corrgram(cabfare[, numeric_index], order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "correlation plot") 
  
  corrgram(cabfare, order = F,
           upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
  

  ##MODELING
  #Sampling
  #1.Non scaled data(scalling is not required as i am using scaled data for my model)
  set.seed(101)
  train_index = sample(1:nrow(cabfare), 0.7*nrow(cabfare))
  data_train = cabfare[train_index,] 
  data_test = cabfare[-train_index,]
  train_cab_final = subset(cabfare, select = c(1,7:12))
  
  summary(train_cab_final)
  str(train_cab_final)
  
  
  #function for error matrics
  mape = function(actual, predict){
    mean(abs((actual-predict)/actual))*100}
  rmse = function(actual, predict){
    sqrt(sum((predict - actual)^2) / nrow(train_cab_final))}
  #mse = function(actual, predict){
    #sqrt(sum((predict - actual)^2) / nrow(train_cab_final)) / mean(actual)}
  
  
  #1 Linear model
  #Model with only distance
  linerModel = lm(fare_amount ~ distance, data = train_cab_final)
  summary(linerModel)
  #print(linerModel)
  #predicting the Fare amount for Test data
  predict_fare_simple=predict(linerModel, newdata = train_cab_final)
  summary(predict_fare_simple)
  #print(predict_fare_simple)
  
  mape(train_cab_final$fare_amount, predict_fare_simple) #25.71183
  rmse(train_cab_final$fare_amount, predict_fare_simple) #3.14
  #mse(train_cab_final$fare_amount, predict_fare_simple) #0.25
  #Multiple R-squared:  0.63,	Adjusted R-squared:  0.63


  #2  DECISSION TREE REGRESSION 
  library(rpart)
  library(MASS)
  library(caTools)
  ##Decison part for regression###
  fit = rpart(fare_amount~.,data=train_cab_final,method="anova")
  summary(fit)
  print(fit)
  #predicting the Fare amount for Test data
  predictions=predict(fit, train_cab_final)
  summary(predictions)
  #print(predictions)
  mape(train_cab_final$fare_amount, predictions) #25.31183
  rmse(train_cab_final$fare_amount, predictions) #2.283257
  #mse(train_cab_final$fare_amount, predictions) #0.268
  
  
  ##3.Random forest
  library(randomForest)
  library(inTrees)
  #model
  model_RF = randomForest(fare_amount ~. , train_cab_final, importance = TRUE, ntree = 500)
  #Error plotting
  plot(model_RF) #my error i decreasing with higher number of trees
  #Checking model by predicting on out of sample data
  predictRF = predict(model_RF, train_cab_final)
  summary(model_RF)
  print(model_RF)
  
  mape(train_cab_final$fare_amount, predictRF) #23.19
  rmse(train_cab_final$fare_amount, predictRF) #1.84
  #mse(train_cab_final$fare_amount, predictRF) #0.21
  #Mean of squared residuals: 4.049305
  #% Var explained: 69.94
  
  plot(train_cab_final$fare_amount, predict_fare_simple, xlab = 'Actual values', ylab = 'Predicted values', main = 'simple liner')
  plot(train_cab_final$fare_amount, predictions, xlab = 'Actual values', ylab = 'Predicted values', main = 'Decision Tree')
  plot(train_cab_final$fare_amount, predictRF, xlab = 'Actual values', ylab = 'Predicted values', main = 'Random forest')
#choosing random forest for my test dataset

#Test Data
apply(cabtest, 2, function(x){sum(is.na(x))})#no missing value
str(cabtest)
cabtest = mutate(cabtest,
                pickup_datetime = ymd_hms(`pickup_datetime`),
                month = as.integer(month(pickup_datetime)),
                year = as.integer(year(pickup_datetime)),
                dayOfWeek = as.integer(wday(pickup_datetime)),
                hour = hour(pickup_datetime),
                hour = as.integer(hour(pickup_datetime))
)
summary(cabtest)

cabtest = cabtest %>% 
  mutate(distance = by(cabtest, 1:nrow(cabtest), function(row) { 
    distHaversine(c(row$pickup_longitude, row$pickup_latitude), c(row$dropoff_longitude,row$dropoff_latitude))/1000}))
summary(cabtest$distance)
str(cabtest)
View(cabtest)

test_cab_final = subset(cabtest, select = c(6:11))
View(test_cab_final)
str(test_cab_final)

predictRF_test = predict(model_RF, test_cab_final)


#Comparing the result
train_cab_final$predict_fare_simple = predict_fare_simple
train_cab_final$predictRF = predictRF
view(test_cab_final)

test_cab_final$fare_amount = predictRF_test

#PLoting the graph for distance and Fare
gplot = ggplot(data=test_cab_final, aes(x=distance, y=fare_amount)) + geom_point()+ geom_line()+ 
  ggtitle("Distance and Fare Plot") +
  xlab("Distance in KM ") + 
  ylab("Fare")
gplot
