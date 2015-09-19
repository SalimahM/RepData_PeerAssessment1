---
<<<<<<< HEAD
title: "Peer Assessment 1"
author: "Salimah Mokhtar"
date: "September 14, 2015"
output: html_document
---
## PART 1 - Loading and preprocessing the data
### (1) Load the required data

The following statement is used to load the data using read.csv().

Note: It is assumed that the file activity.csv is in the current working directory. File can be downloaded from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

```r
Activity_Monitoring_Data <- read.csv('activity.csv', header=TRUE, sep = ",")
```

### (2) Process / transform the data into a format suitable for analysis.

The date field is converted to Date class, interval field to Factor class, weekdays from date attribute, compute day type.

```r
Activity_Monitoring_Data$date <- as.Date(Activity_Monitoring_Data$date, format = "%Y-%m-%d")
Activity_Monitoring_Data$interval <- as.factor(Activity_Monitoring_Data$interval)

# Compute the weekdays from the date attribute
Activity_Monitoring_Data <- data.frame(date=Activity_Monitoring_Data$date, 
                 weekday=tolower(weekdays(Activity_Monitoring_Data$date)), 
                 steps=Activity_Monitoring_Data$steps, 
                 interval=Activity_Monitoring_Data$interval)

# Compute the day type (indicate either weekend or weekday)
Activity_Monitoring_Data <- cbind(Activity_Monitoring_Data, 
            daytype=ifelse(Activity_Monitoring_Data$weekday == "saturday" | 
                             Activity_Monitoring_Data$weekday == "sunday", "weekend","weekday"))

# Create the final data frame
Activity_Data <- data.frame(date=Activity_Monitoring_Data$date, 
                       weekday=Activity_Monitoring_Data$weekday, 
                       daytype=Activity_Monitoring_Data$daytype, 
                       interval=Activity_Monitoring_Data$interval,
                       steps=Activity_Monitoring_Data$steps)

str(Activity_Data) #  check the final dataset using str() method
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ weekday : Factor w/ 7 levels "friday","monday",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ daytype : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
```

```r
# Clear the workspace
rm(Activity_Monitoring_Data)
```
Summary of the final dataset.

```r
library(xtable)
summary(Activity_Data)
```

```
##       date                 weekday        daytype         interval    
##  Min.   :2012-10-01   friday   :2592   weekday:12960   0      :   61  
##  1st Qu.:2012-10-16   monday   :2592   weekend: 4608   5      :   61  
##  Median :2012-10-31   saturday :2304                   10     :   61  
##  Mean   :2012-10-31   sunday   :2304                   15     :   61  
##  3rd Qu.:2012-11-15   thursday :2592                   20     :   61  
##  Max.   :2012-11-30   tuesday  :2592                   25     :   61  
##                       wednesday:2592                   (Other):17202  
##      steps       
##  Min.   :  0.00  
##  1st Qu.:  0.00  
##  Median :  0.00  
##  Mean   : 37.38  
##  3rd Qu.: 12.00  
##  Max.   :806.00  
##  NA's   :2304
```


## PART 2 - What is the mean total number of steps taken per day?
For this part of the assignment, the missing values in the dataset are ignored.
First, we calculate the total number of steps taken per day.

```r
# Compute the total number of steps each day (NA values removed)
total_steps <- aggregate(Activity_Data$steps,by=list(Activity_Data$date), FUN=sum, na.rm=TRUE)

# Rename the attributes
names(total_steps) <- c("date", "total")
```

Then, we make a histogram of the total number of steps taken each day.

```r
library(ggplot2)

ggplot(total_steps, aes(date, total)) + 
  geom_bar(stat = "identity", colour = "blue",fill = "dark blue", width = 0.5) + 
  labs(title = "Figure 1 - Total Number of Steps Taken Each Day\n(NA Removed)", x = "Date (October 2012 - November 2012)", y = "Number of Steps Taken") +
  theme(axis.text.x = element_text(angle = 90,hjust = 0))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Lastly, we calculate the mean and median of the total number of steps taken per day.

```r
totalsteps_mean <- mean(total_steps$total)
totalsteps_median <- median(total_steps$total)
```


The mean is **9354.23** and the median is **10395**.
  
       
## PART 3 - What is the average daily activity pattern?
We calculate the aggregation of steps by intervals of 5-minutes and convert the intervals as integers and save them in a data frame called *steps_by_interval*.


```r
steps_by_interval <- aggregate(Activity_Data$steps,by = list(interval= Activity_Data$interval), FUN=mean, na.rm=TRUE)
steps_by_interval$interval <- 
        as.integer(levels(steps_by_interval$interval)[steps_by_interval$interval])
colnames(steps_by_interval) <- c("interval", "steps")
```

A time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) is made.

```r
library(ggplot2)
ggplot(steps_by_interval, aes(x=interval, y=steps)) + geom_line(color="dark green", size=1) + labs(title="Figure 2 - A Time-Series Plot of Average Daily Activity Pattern", x="5-Minute Interval", y="Average Number of Steps Taken") + theme_bw()
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_by_interval[which.max(steps_by_interval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

The **835^th^** (8:35am) interval has maximum **206** steps.


## PART 4 - Inputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

(1) The total number of missing values in the dataset can be calculated using is.na() method to check whether the value is missing or not and then summing the total of missing values.


```r
missing_values <- sum(is.na(Activity_Data$steps))
```

The total number of missing values in the dataset are **2304**.

(2) Next, we will devise a strategy for filling in all the missing values in the dataset.

To populate the missing values, we will replace them with the mean value of its 5-minute interval.


```r
# Find the NA position in the dataset
na_position <- which (is.na(Activity_Data$steps))

# Create the mean vector
mean_fill <- rep(mean(Activity_Data$steps, na.rm=TRUE, time=length(na_position)))

# Replace the NA position with mean
Activity_Data[na_position, "steps"] <- mean_fill
```


To check if there are any missing values remaining or not, we compute the following:


```r
missing_val <- sum(is.na(Activity_Data$steps))
```

Since the output is **0**, this indicate that there is **NO MISSING VALUES**.


(3) Showing a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# Display the first few rows of new dataset, where all NA are replaced with mean
head(Activity_Data)
```

```
##         date weekday daytype interval   steps
## 1 2012-10-01  monday weekday        0 37.3826
## 2 2012-10-01  monday weekday        5 37.3826
## 3 2012-10-01  monday weekday       10 37.3826
## 4 2012-10-01  monday weekday       15 37.3826
## 5 2012-10-01  monday weekday       20 37.3826
## 6 2012-10-01  monday weekday       25 37.3826
```

(4) Making a histogram of the total number of steps taken each day and calculating  the mean and median total number of steps taken per day. 
Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
clean_data <- Activity_Data
for (i in 1:nrow(clean_data)) {
  if(is.na(clean_data$steps[i])) {
    clean_data$steps[i] <- agint[clean_data$interval[i] == agint$interval, ]$steps
  }
}
agg_clean_data <- aggregate(steps~date,data=clean_data,sum)
library(ggplot2)
ggplot(agg_clean_data, aes(date, steps)) + 
  geom_bar(stat = "identity", colour = "red",fill = "dark red", width = 0.5) + 
  labs(title = "Figure 3 - Total Number of Steps Taken Each Day\n(Without Missing Data)", x = "Date (October 2012 - November 2012)",y = "Number of Steps Taken") + theme(axis.text.x = element_text(angle = 90,hjust = 0))
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 


```r
new_mean <- mean(agg_clean_data$steps)
new_median <- median(agg_clean_data$steps)
```

The new mean is **10766.189** , and the new median is **10766.189**.

These values of mean and median differ greatly from the estimates from the first part of the assignment (See Figure 1). The impact of inputing the missing values is to have more data, hence to obtain a bigger mean and median value.

## PART 5 - Are there differences in activity patterns between weekdays and weekends?


We do this comparison with the table with filled-in missing values.

1. Augment the table with a column that indicates the day of the week


```r
head (Activity_Data)
```

```
##         date weekday daytype interval   steps
## 1 2012-10-01  monday weekday        0 37.3826
## 2 2012-10-01  monday weekday        5 37.3826
## 3 2012-10-01  monday weekday       10 37.3826
## 4 2012-10-01  monday weekday       15 37.3826
## 5 2012-10-01  monday weekday       20 37.3826
## 6 2012-10-01  monday weekday       25 37.3826
```

2. Subset the table into two parts - weekends (Saturday and Sunday) and weekdays (Monday through Friday).
3. Tabulate the average steps per interval for each data set.
4. Plot the two data sets side by side for comparison.


```r
# Load the lattice graphical library
library(lattice)

# Compute the average number of steps taken, averaged across all daytype variable
steps_by_interval <- aggregate(Activity_Data$steps, by=list(Activity_Data$daytype, Activity_Data$weekday, Activity_Data$interval), mean)

# Rename the attributes
names(steps_by_interval) <- c("DayType", "Weekday", "Interval", "Steps")

# Display the first few rows of the steps_by_interval data frame:
head (steps_by_interval)
```

```
##   DayType  Weekday Interval    Steps
## 1 weekday   friday        0 8.307244
## 2 weekday   monday        0 9.418355
## 3 weekend saturday        0 4.672825
## 4 weekend   sunday        0 4.672825
## 5 weekday thursday        0 9.375844
## 6 weekday  tuesday        0 0.000000
```

Creating time-series plot.

```r
xyplot(Steps ~ Interval | DayType, data=steps_by_interval, 
       type="l", lwd=1,
       main="Figure 4 - Activity Patterns for Weekdays and Weekends",
       xlab="Interval", 
       ylab="Number of Steps", 
       layout=c(1,2))
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png) 

There is a difference in activity patterns between weekdays and weekends. 
Weekday activity peaks at 835 mins, with lower peaks after that. 
Weekend, on the other hand, though does not peak as high as during weekday, showed a more uniform period of activity from 800 mins to 2000 mins.
=======
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data



## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
>>>>>>> 80edf39c3bb508fee88e3394542f967dd3fd3270
