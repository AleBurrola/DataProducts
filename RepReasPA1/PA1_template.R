## 1. Loading and preprocessing the data
data <- read.csv("activity.csv",header = TRUE)
library(doBy)
library(dplyr)
library(lattice)


## 2. What is mean total number of steps taken per day?

## 2.a total number of steps per day
tabla <- summaryBy(steps ~ date, data = data, FUN = list(sum),na.rm=TRUE)

## 2.b histogram of the total number of steps per day
hist(tabla$steps.sum,col="green",main="Number of Steps, Original Data",xlab="")

## 2.c calculate and report the mean and median of the total number of steps per day
mean(tabla$steps.sum,na.rm=TRUE)
median(tabla$steps.sum,na.rm=TRUE)

## 3. What is the average daily activity pattern

## 3.a time series plot of the 5 minute interval (x-axis)
## and the average number of steps taken, averaged across all days (y-axis)
intervalos <- summaryBy(steps ~ interval, data = data, FUN = list(mean,max),na.rm=TRUE)
plot(intervalos$steps.mean~intervalos$interval,type="l",xlab="intervals",ylab="steps mean")

## Which 5-minute interval cointains the max amount of steps
maxsteps <- max(intervalos$steps.max)
sel_intervalos <- subset(intervalos, intervalos$steps.max == maxsteps)
print(sel_intervalos)
print(sel_intervalos$interval)


## 4. Imputing missing values

## 4.a Calculate and report the total number of missing values in the dataset
rows <- length(which(is.na(data$steps)))
## 4.b Devise a strategy for filling in all of the missing values in the dataset
## answer: fill missing values of steps with average of daily intervals
## 4.c Create a new dataset that is equal to the original dataset but with the missing data filled in
## 4.c.i subset original data taking out nas
select_no_nas <- na.omit(data)
## 4.c.ii subset original data only with nas
select_nas <- subset(data, is.na(data$steps))
## 4.c.iii replace subsetted nas with averaged steps per intervals in select_nas subset
reemplazo <- data.frame(select_nas,intervalos$steps.mean)
names(reemplazo) <- c("setps0","date","interval","steps")
reemplazo <- select(reemplazo,steps,date,interval)
## 4.c.iv rbind subset original data without nas with replaced subsetted nas
data2 <- rbind(select_no_nas,reemplazo)
## 4.c.v arrange by date and interval
data2 <- arrange(data2,date,interval)

## 4.d Make a histogram of the total number of steps taken each day 
## and Calculate and report the mean and median total number of steps taken per day
## 4.d.i histogram
tabla2 <- summaryBy(steps ~ date, data = data2, FUN = list(sum))
hist(tabla2$steps.sum,col="green",main="Number of Steps, Replaced NAs",xlab="")
## 4.d.ii mean and median
mean(tabla2$steps.sum,na.rm=TRUE)
median(tabla2$steps.sum,na.rm=TRUE)

## Do these values differ from the estimates from the first part of the assignmen
## What is the impact of imputing missing data on the estimates of the total daily number of steps?


## 5. Are there differences in activity patterns between weekdays and weekends?

## 5.a Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
data2$day <- as.POSIXlt(data2$date)$wday
findesemana <- subset(data2, data2$day == 6 | data2$day == 0)
findesemana$weekday <- c("weekend")
entresemana <- subset(data2, data2$day > 0 & data2$day < 6)
entresemana$weekday <- c("weekday")
data3 <- rbind(entresemana,findesemana)
data3 <- arrange(data3,date,interval)

## 5.b Make a panel plot containing a time series plotof the 5-minute interval (x-axis)
## and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
tabla3 <- summaryBy(steps ~ interval + weekday, data = data3, FUN = list(mean))
xyplot(tabla3$steps.mean ~ tabla3$interval | tabla3$weekday, layout = c(1,2),type="l",xlab="Interval",ylab="Number of Steps")
