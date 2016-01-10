# Course Project 1 for Reproducible Research - R Piazza Jan 9 2016

#necessary libraries
library(ggplot2)

#Code for reading in the dataset and/or processing the data

#Read in the data
ActivityRaw <- read.csv(unzip('activity.zip'))
act <- ActivityRaw
#reformat the dates to date format

act$date <- as.Date(as.character(act$date))

#Histogram of the total number of steps taken each day
SumOfSteps <- aggregate(steps ~ date, act, sum)
qplot(steps, data = SumOfSteps)

#What is mean total number of steps taken per day?
MeanOfSteps <- aggregate(steps ~ date, act, mean)
qplot(date, steps, data = MeanOfSteps, geom = "line")
#What is the median?
MedianOfSteps <- aggregate(steps ~ date, act, median, na.rm = TRUE)
#median is exclusively zero- data says that people are normally not walking
qplot(date, steps, data = MedianOfSteps)



#What is the average daily activity pattern?
#Time series plot of the average number of steps taken
AvgActivityPattern <- aggregate(steps ~ interval, act, mean)
qplot(interval, steps,data = AvgActivityPattern, geom = "line")



#The 5-minute interval that, on average, contains the maximum number of steps
MaxIntervalAverage <- AvgActivityPattern$interval[AvgActivityPattern$steps== max(AvgActivityPattern$steps)]
#Max interval is 835 

#Code to describe and show a strategy for imputing missing data

#first find where we have NA values
mean(is.na(act$steps)) #.1311475
mean(is.na(act$date)) #0
mean(is.na(act$interval))#0
#how many NA's are we talking about?
sum(is.na(act$steps)) #2304

#13% of the steps values are NA or 2304 rows, so steps values are the only ones we have to imput


#for all NA values, I intend to take the overall average value for that 5 min period and replace the NA

#I'll go line by line, taking the first NA value found, 
#lookup the interval associated, 
#lookup that associated average activity in ActivityPattern and 
#replace the corresponding step value

#create the new dataset
ActImputed <- act
while (mean(is.na(ActImputed$steps) != 0)) {
  ActImputed$steps[is.na(ActImputed$steps)][1] <- ActivityPattern$steps[ActivityPattern$interval== ActImputed$interval[is.na(ActImputed$steps)][1]]
}

#New histogram of the total number of steps taken each day
SumOfStepsImputed <- aggregate(steps ~ date, ActImputed, sum)
qplot(steps, data = SumOfStepsImputed)


#Histogram of the total number of steps taken each day after missing values are imputed
MeanOfStepsImputed <- aggregate(steps ~ date, ActImputed, mean)
qplot(steps, data = MeanOfStepsImputed)
#What is the median?
MedianOfStepsImputed <- aggregate(steps ~ date, act, median, na.rm = TRUE)
#medians are still exclusively zero- data didn't change much from imputed values
qplot(date, steps, data = MedianOfStepsImputed)

#The median doesn't change from 0 and we most notable add data points for the mean at the beginning and end of the day

#looking back, I think the assignment actually wants the overall mean and median for the entire dataset not just by day so:
OverallAvgStepsImputed <- mean(SumOfStepsImputed$steps) #10766.19 Same as before
OverallMedStepsImputed <- median(SumOfStepsImputed$steps) #10766.18 Now went up slightly and is also the same as the mean

#for fun, let's make a plot of the imputed values (blue line) with the original data points (orange points)
plot(MeanOfStepsImputed$date, MeanOfStepsImputed$steps, type = "l",col = 'blue')
points(MeanOfSteps$date, MeanOfSteps$steps, col = 'orange')

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

#add another column to Activity Imputed called Weekday and set is as weekday or weekend
ActImputed$weekday[weekdays(ActImputed$date) == 'Saturday' | weekdays(ActImputed$date) == 'Sunday'] <- 'weekend'
ActImputed$weekday[weekdays(ActImputed$date) != 'Saturday' & weekdays(ActImputed$date) != 'Sunday'] <- 'weekday'
#change all weekends and weekdays to factors
ActImputed$weekday <- as.factor(ActImputed$weekday)
#create the plot and color according to weekday

meanWeekday <- aggregate(steps ~ interval+weekday, data = ActImputed, FUN = mean)
qplot(x = interval, y = steps, data = meanWeekday, facets = weekday~., geom = "line", col = weekday)
#All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
