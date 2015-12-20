---
title: "PA1_template"
output: html_document
---

##Loading and preprocessing the data


Show any code that is needed to:


1. Load the data (i.e. read.csv())


```r
unzip("repdata-data-activity.zip")
act.db <- read.csv("activity.csv")
```


2. Process/transform the data (if necessary) into a format suitable for your analysis

A brief inspection of the data shows that the column "date" has been classified as "factor", which will need to be corrected. 


```r
act.db$date <- as.Date(act.db$date, "%Y-%m-%d")
```



##What is mean total number of steps taken per day?


For this part of the assignment, you can ignore the missing values in the dataset.


1. Calculate the total number of steps taken per day


```r
steps.day <- tapply(act.db$steps, act.db$date, sum, na.rm=TRUE)
```


2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
library(lattice)
hist(steps.day, main = "Steps taken each day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 


3. Calculate and report the mean and median of the total number of steps taken per day


```r
steps.mean <- mean(steps.day)
steps.median <- median(steps.day)
```

- Mean: 9354.23
- Median: 1.0395 &times; 10<sup>4</sup>


##What is the average daily activity pattern?


1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

We need to calculate the steps mean for each interval, which will be the "y" axis in the plot, as follows:


```r
mean.interval <- tapply(act.db$steps, act.db$interval, mean, na.rm = TRUE)
plot(x = rownames(mean.interval), y = mean.interval, type = "l", xlab = "5-min interval", ylab = "Average steps", main = "Average number of steps taken")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max.int <- which.max(mean.interval)
```

- Average 5-minute interval with max number of steps: 835



##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Given that only the "steps" column contains NAs, we can calculate the number of missing values by a simple approximation:


```r
count.na <- sum(is.na(act.db))
```

- Number of missing values: 2304


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
db.l <- length(act.db$steps)

act.db$steps.c <- NULL

for(i in 1:(db.l-1)) {
    if(is.na(act.db$steps[i])) {
        act.db$steps.c[i] <- mean.interval[which(rownames(mean.interval)==act.db$interval[i])]
    }
    if(!is.na(act.db$steps[i])) {
         act.db$steps.c[i] <- act.db$steps[i]
    }
}
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

When running the last code chunk I created a new column with the corrected data "steps.c". The new dataset would just be a subset fot he columns in the original:


```r
library(dplyr)
act.db.correct <- select(act.db, date:steps.c)
act.db.correct <- act.db.correct[,c(3,1,2)]
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
steps.day.c <- tapply(act.db$steps.c, act.db$date, sum, na.rm=TRUE)
hist(steps.day.c, main = "Steps taken each day (corrected)")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
steps.mean.c <- mean(steps.day.c)
steps.median.c <- median(steps.day.c)
```

- Mean (corrected): 1.07662 &times; 10<sup>4</sup>
- Median (corrected): 1.076619 &times; 10<sup>4</sup>

After replacing, the mean and the median become very similar. The biggest difference vs the dataset with missing values is the value of the mean. 



##Are there differences in activity patterns between weekdays and weekends?


For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.


1. Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.


```r
act.db.correct$date.type <- ifelse(as.POSIXlt(act.db.correct$date)$wday %in% c(0,6),'weekend', 'weekday')
act.db.correct$date.type <- factor(act.db.correct$date.type)
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
steps.day.w <- aggregate(steps.c ~ interval + date.type, data = act.db.correct, mean)
xyplot(steps.c ~ interval | date.type, steps.day.w, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
