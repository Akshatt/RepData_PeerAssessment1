---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activity <- read.csv(unzip("activity.zip"))
activity$date <- as.Date(activity$date, "%Y-%m-%d")
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```


## What is mean total number of steps taken per day?

```r
daily_steps <- data.frame(with(activity, tapply(steps, date, sum, na.rm=TRUE)))
daily_steps$Date <- rownames(daily_steps)
names(daily_steps)[[1]] <- "Steps"
hist(daily_steps$Steps, xlab="Total Steps", col="blue", main="Total Number of Steps Taken Each Day", breaks=seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



```r
mean_steps <- mean(daily_steps$Steps)
median_steps <- median(daily_steps$Steps)
```
Mean of total number of steps taken per day: 9354.2295082  
Median of total number of steps taken per day: 10395


## What is the average daily activity pattern?

```r
average_interval_steps <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(average_interval_steps) <- c("Interval", "Mean")
with(average_interval_steps, plot(Interval, Mean, type="l", col="green", lwd=2,
                                    xlab="Interval",
                                    ylab="Average number of Steps",
                                    main="Average number of Steps per Interval"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
interval <- average_interval_steps[which.max(average_interval_steps$Mean),
                                   ]$Interval
```
The interval 835 has the maximum number of steps on average across all days. 


## Imputing missing values


```r
count_na <- sum(apply(activity, 1, anyNA))
new_activity <- activity

# Substituting NA with mean of that 5 minute interval
for(i in 1:length(new_activity$steps)){
    if (is.na(new_activity$steps[i])){
        condition <- average_interval_steps$Interval == new_activity$interval[i]
        new_activity$steps[i] <- average_interval_steps$Mean[condition]      
    } 
}
```
Total number of rows with missing values/NA is 2304


```r
daily_steps <- data.frame(with(new_activity, tapply(steps, date, sum, na.rm=TRUE)))
daily_steps$Date <- rownames(daily_steps)
names(daily_steps)[[1]] <- "Steps"
hist(daily_steps$Steps, xlab="Total Steps", col="blue", main="Total Number of Steps Taken Each Day(No missing values)", breaks=seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
new_mean <- format(round(mean(daily_steps$Steps), 2), nsmall=2) 
new_median <- format(round(median(daily_steps$Steps), 2), nsmall=2)
```
Mean of total number of steps taken per day (No missing values): 10766.19  
Median of total number of steps taken per day (No missing values): 10766.19  

```r
diff_mean <- as.numeric(new_mean) - mean_steps
diff_median <- as.numeric(new_median) - median_steps
```

We see a significant rise in both mean and median of the total steps.   
Both mean and median have increased by 1411.9604918 and 371.19

## Are there differences in activity patterns between weekdays and weekends?

```r
wdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
new_activity$weekday <- factor((weekdays(new_activity$date) %in% wdays), 
                              levels = c(FALSE,TRUE), 
                              labels = c("weekend","weekday"))
```



```r
library(ggplot2)

average_activity_by_days <- aggregate(steps~interval + weekday, new_activity, mean, na.rm = TRUE)
ggplot(average_activity_by_days, aes(x=interval, y=steps, color=weekday))+
    geom_line()+
    labs(title = "Daily Average steps by Weekdays", 
         x = "Interval", y = "Average number of steps") +
    facet_grid(weekday~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->






