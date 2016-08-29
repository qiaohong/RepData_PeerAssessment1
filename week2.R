setwd("/Users/qiaohong/Desktop/Coursera/reproduce/RepData_PeerAssessment1")
file <- unzip("activity.zip")
data <- read.csv(file)
total_steps_day <- aggregate(data$steps, by= list(data$date), na.rm = T, FUN = sum)
data_clean <-subset(data, !is.na(data$steps) )
total_per_day <- tapply(data_clean$steps, data_clean$date, sum)
hist(total_per_day, xlab = "Total steps per day", ylab="Frequency", breaks = 10, main="Histogram for total steps per day")
summary(total_per_day)

avg_per_interval <- tapply(data_clean$steps, data_clean$interval, mean)



plot(avg_per_interval, 
     type="l", 
     ylab="Average steps taken", 
     xlab="Interval #", 
     main="Average steps taken per 5-min interval, all dates")

names(avg_per_interval[which.max(avg_per_interval)])

sum(is.na(data$steps))
sum(is.na(data$steps))/nrow(data)

## Imputing NA's
## First calculate the mean steps taken per 5-min interval
mean_per_interval <- aggregate(data$steps, by= list(data$interval), na.rm = T, FUN = mean)
## Loop through each row in data, imputing the NA value according to mean_per_interval
data_imputed <- data
for (i in 1:nrow(data)){
  if(is.na(data$steps[[i]])){
    ## inner join
    temp_data_impute <- merge(data[i,], mean_per_interval, by.x = "interval", by.y = "Group.1")
    ## populate x into steps
    data_imputed$steps[[i]] <- temp_data_impute$x
    ## clean up
    rm(temp_data_impute)
    }
}

total_per_day_im <- tapply(data_imputed$steps, data_imputed$date, sum)
hist(total_per_day_im, xlab = "Total steps per day", ylab="Frequency", breaks = 10, main="Histogram for total steps per day")
summary(total_per_day_im)

data_imputed$as_date <- as.Date(data_imputed$date, "%Y-%m-%d")
data_imputed$weekday <- weekdays(data_imputed$as_date)
weekend <- c("Saturday", "Sunday")

data_imputed$is_weekend <- data_imputed$weekday %in% weekend + 1
data_imputed$is_weekend_factor <- factor(data_imputed$is_weekend, labels = c("Weekend", "Weekday"))

avg_per_interval_weekday <- aggregate(data_imputed$steps, 
                                      by = list(weekend = data_imputed$is_weekend_factor, interval = data_imputed$interval),
                                      FUN = mean) 

library("lattice")
xyplot(x ~ interval | weekend, data = avg_per_interval_weekday,
       type = "l",
       xlab = "Interval", ylab = "Number of steps",
       layout = c(1,2)
       )


