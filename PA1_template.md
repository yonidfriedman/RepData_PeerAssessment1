# Personal Activity Analysis
Jonathan Friedman  
December 4, 2016  

## Loading and Preprocessing the Data


```r
theURL <- ("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")
z <- tempfile()
download.file(theURL, z)
unzip(z)
activity <- read.csv("activity.csv")
activity$date <- as.Date(as.character(activity$date), "%Y-%m-%d")
```

## What is the mean total number of steps taken per day?

Calculate the total number of steps taken per day


```r
library(dplyr)
steps_day <- activity %>%  group_by(date) %>% summarize(sum = sum(steps, na.rm = TRUE))
```

Then, make a histogram of the total number of steps taken each day

```r
hist(steps_day$sum, breaks = 10, col = "blue", main = "Histogram of steps by date", ylab = "Frequency", xlab = "Steps Taken")
```

![](PA1_template_files/figure-html/Histogram-1.png)<!-- -->

Then, calculate and report the median and the mean of steps taken per day

```r
steps_ave <- round(mean(steps_day$sum))
steps_median <- median(steps_day$sum)
```
The mean steps taken per day is 9354
The median steps taken per day is 10395


## What is the average daily activity pattern?

Make a time-series plot of steps taken by interval across days
First, calculate the average number 


```r
steps_interval <- activity %>% group_by(interval) %>% summarize(mean = mean(steps, na.rm = TRUE))
```

Then, plot the line graph

```r
with(steps_interval, plot(interval, mean, type = "l", main = "Average Steps Taken by Interval", ylab = "Average Steps", xlab = "Interval"))
```

![](PA1_template_files/figure-html/Line Graph-1.png)<!-- -->

Report which interval had the highest average steps across the time period


```r
max <- which.max(steps_interval$mean)
```

Inerval 104 has the highest average steps taken across the time period.

## Imputing Missing Values
Calculate and report the total number of missing values in the data set (number of rows with NA's)


```r
missing <- activity[is.na(activity$steps), ]
nrow(missing)
```

```
## [1] 2304
```


For NA's, assume appropriate values are the average steps for that interval across the time period


```r
activity$steps <- ifelse(is.na(activity$steps), steps_interval$mean[steps_interval$interval %in% activity$interval], activity$steps)
```

Calculate the total number of steps taken each (including inputed values)


```r
steps_day_complete <- activity %>%  group_by(date) %>% summarize(sum = sum(steps))
```

Plot the histogram

```r
hist(steps_day_complete$sum, breaks = 10, col = "blue", main = "Total Steps Taken By Day", ylab = "Frequency", xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/Plot Histogram-1.png)<!-- -->

Calculate and report the median and the mean of steps taken per day

```r
steps_ave1 <- round(mean(steps_day_complete$sum))
steps_median1 <- median(steps_day_complete$sum)
```

The average number of steps taken per day is 1.0766\times 10^{4}
The median number of steps taken per day is 1.0766189\times 10^{4}


Since the inputed values are the average values, the average steps per day is unchanged. The median, however, changes slightly.

## Are there any differences in activity patterns between weekdays and weekends?

Create a new factor variable in the data set for weekdays and weekends


```r
activity1 <- activity
activity1$day <- ifelse(weekdays(activity1[, 2]) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
activity1$day <- as.factor(activity1$day)
```

Segment mean steps by interval for weekdays and weekends


```r
weekday_interval <- activity1 %>% filter(day == "Weekday") %>% group_by(interval) %>% summarize(mean = mean(steps))
weekend_interval <- activity1 %>% filter(day == "Weekend") %>% group_by(interval) %>% summarize(mean = mean(steps))
```


Finally, plot the line graphs


```r
par(mfrow= c(1,2))
plot(weekday_interval$mean, type = "l", main = "Weekday Average Steps by Interval", xlab = "Interval", ylab = "Average Steps")
plot(weekend_interval$mean, type = "l", main = "Weekend Average Steps by Interval", xlab = "Interval", ylab = "Average Steps")
```

![](PA1_template_files/figure-html/Panel Plot-1.png)<!-- -->


