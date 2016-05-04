# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data <- read.csv(unz("activity.zip", "activity.csv"))
data$date <- as.Date(data$date)
```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
library(dplyr)
datebase <- group_by(data,date)
datesum  <- as.data.frame(summarize(datebase, tot_steps = sum(steps, na.rm=TRUE)))
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(datesum$tot_steps, main="Step Frequency", xlab = "Total Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
options("scipen"=100)
datesummean <- summary(datesum$tot_steps)[4]
datesummedian <- summary(datesum$tot_steps)[3]
summary(datesum$tot_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

For the total number of steps taken per day, the mean is 9354 and the median is 10400.


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(dplyr)
dateinv <- group_by(data,interval)
dateinvavg  <- as.data.frame(summarize(dateinv, avg_steps = mean(steps, na.rm=TRUE)))
plot(dateinvavg$interval,dateinvavg$avg_steps,type="l", xlab="Interval", ylab="Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxavg <- subset(dateinvavg, avg_steps == max(dateinvavg$avg_steps))
maxinv <- maxavg[1]
maxsteps <- round(maxavg[2], digits = 2)
maxhour <- floor(maxinv/60)
maxmin <- maxinv - (maxhour * 60)
```

The max number of average steps is taken at the **835** interval, around **13:55**, and peaks at **206.17 steps**.

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
data2 <- merge(data, dateinvavg, by="interval")
data2$newsteps <- ifelse(is.na(data2$steps) == TRUE, round(data2$avg_steps,0),data2$steps)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data3 <- data2[,c(5,3,1)]
names(data3) <- names(data)
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
datebase3 <- group_by(data3,date)
datesum3  <- as.data.frame(summarize(datebase3, tot_steps = sum(steps, na.rm=TRUE)))
par(mfrow = c(1, 2))
hist(datesum$tot_steps, main="Step Frequency", xlab = "Total Steps Taken Each Day")
hist(datesum3$tot_steps, main="Step Frequency Normalized NA", xlab = "Total Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Summary with NAs


```r
summary(datesum$tot_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

Summary with NAs replaced with interval average


```r
summary(datesum3$tot_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10760   10770   12810   21190
```

```r
options("scipen"=100)
datesum3mean <- summary(datesum3$tot_steps)[4]
datesum3median <- summary(datesum3$tot_steps)[3]
```

For the total number of steps taken per day when adjusted for the NAs, the mean has changed from **9354 to 10770** and the median changed from **10400 to 10760**.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data3$weekpart <- ifelse(weekdays(data3$date) == c("Saturday","Sunday"), "weekend","weekday")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
data3inv <- group_by(data3,interval, weekpart)
data3invavg  <- as.data.frame(summarize(data3inv, avg_steps = mean(steps, na.rm=TRUE)))
library(lattice)
xyplot (avg_steps ~ interval | weekpart, data = data3invavg, layout = c(1,2), type = "l", xlab = "Inverval", ylab = "Number of Average Steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
