Peer Assessment 1
========================================================

## Loading and preprocessing the data

Show any code that is needed to:

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
steps1 <- read.csv("./activity.csv")
head(steps1)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the mean and median total number of steps taken per day


```r
steps2 <- steps1[complete.cases(steps1), ]
hist(steps2[, 1])
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 



```r
stepsMean <- mean(steps1[, 1], na.rm = TRUE)
stepsMedian <- median(steps1[, 1], na.rm = TRUE)
```


The mean is 37.3826 and the median is 0.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervalMean <- tapply(steps2[, 1], steps2[, 3], mean)
intervalNames <- names(tapply(steps2[, 1], steps2[, 3], mean))
maxAvgInterval <- max(intervalMean)
posMax <- grep(maxAvgInterval, intervalMean)
posMax1 <- intervalNames[posMax]
```


The 5-minute interval that contains the maximum number of steps is 835.


```r
plot(intervalMean, type = "l", xaxt = "n")
axis(1, at = 1:length(intervalNames), labels = intervalNames)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
valuesMissing <- nrow(steps1) - nrow(steps2)
```


The total number of missing values in the dataset is 2304.


```r
newIntCol <- as.numeric(vector())
intervalCount <- 1:nrow(steps1)
steps12 <- steps1
intervalMean1 <- as.vector(intervalMean)
intervalMeanRep <- rep(intervalMean1, 61)
steps3 <- cbind(steps12, intervalMeanRep)
for (i in intervalCount) {
    if (is.na(steps1[i, 1]) == TRUE) {
        steps12[i, 1] <- steps3[i, 4]
    }
}

stepsMean3 <- mean(steps12[, 1])
stepsMedian3 <- median(steps12[, 1])
diffMean <- stepsMean3 - stepsMean
diffMedian <- stepsMedian3 - stepsMedian
```


The new mean is 37.3826 and new median is 0. The difference in mean is 0 and difference in median is 0.


```r
hist(steps12[, 1])
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


When using the mean of the 5-minute interval to fill in the missing data, there is no difference in mean or median.

## Are there differences in activity patterns between weekdays and weekends?

### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part. 

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data


```r
library(chron)
library(lattice)
steps12 <- steps1
stepsDate <- as.Date(steps12$date, "%Y-%m-%d")
stepsDate1 <- is.weekend(stepsDate)
stepsDate2 <- sub(FALSE, "weekday", stepsDate1)
stepsDate3 <- sub(TRUE, "weekend", stepsDate2)
steps13 <- cbind(steps12, stepsDate3)
steps13[, 3] <- as.numeric(steps13[, 3])

stepsWD <- steps12[grep("weekday", stepsDate1), ]
stepsWE <- steps12[grep("weekend", stepsDate1), ]
weekdayMean <- tapply(stepsWD[, 1], stepsWD[, 3], mean)
weekendMean <- tapply(stepsWE[, 1], stepsWE[, 3], mean)
weekMean <- c(weekdayMean, weekendMean)

weekendChar <- rep("weekend", 288)
weekdayChar <- rep("weekday", 288)
weekendMean1 <- as.vector(weekendMean)
weekdayMean1 <- as.vector(weekdayMean)
tableWeek <- c(weekdayMean1, weekendMean1)
tableDay <- c(weekdayChar, weekendChar)
stepsTable1 <- as.data.frame(cbind(tableWeek, tableDay, intervalNames), stringsAsFactors = FALSE)
stepsTable1[, 1] <- as.numeric(stepsTable1[, 1])
```

```
## Warning: NAs introduced by coercion
```

```r
stepsTable1[, 3] <- as.numeric(stepsTable1[, 3])
```

```
## Error: undefined columns selected
```

```r
x <- stepsTable1[, 3]
```

```
## Error: undefined columns selected
```

```r
y <- stepsTable1[, 1]
f <- stepsTable1[, 2]
xyplot(y ~ x | f, layout = c(1, 2), type = "l")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-91.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-92.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-93.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-94.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-95.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-96.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-97.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-98.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-99.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-910.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-911.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-912.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-913.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-914.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-915.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-916.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-917.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-918.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-919.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-920.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-921.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-922.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-923.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-924.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-925.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-926.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-927.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-928.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-929.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-930.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-931.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-932.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-933.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-934.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-935.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-936.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-937.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-938.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-939.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-940.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-941.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-942.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-943.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-944.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-945.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-946.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-947.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-948.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-949.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-950.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-951.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-952.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-953.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-954.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-955.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-956.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-957.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-958.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-959.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-960.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-961.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-962.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-963.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-964.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-965.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-966.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-967.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-968.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-969.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-970.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-971.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-972.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-973.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-974.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-975.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-976.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-977.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-978.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-979.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-980.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-981.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-982.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-983.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-984.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-985.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-986.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-987.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-988.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-989.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-990.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-991.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-992.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-993.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-994.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-995.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-996.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-997.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-998.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-999.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9100.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9101.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9102.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9103.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9104.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9105.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9106.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9107.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9108.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9109.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9110.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9111.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9112.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9113.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9114.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9115.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9116.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9117.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9118.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9119.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9120.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9121.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9122.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9123.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9124.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9125.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9126.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9127.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9128.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9129.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9130.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9131.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9132.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9133.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9134.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9135.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9136.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9137.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9138.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9139.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9140.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9141.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9142.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9143.png) ![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9144.png) 

