# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
ad <- read.csv("activity.csv");

# We'll be using these libraries in some of the chunks
library(plyr)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```


## What is mean total number of steps taken per day?

```r
stepsPerDay = aggregate(ad$steps, list(date = ad$date), function(x) {sum(x, na.rm = TRUE)})

hist(stepsPerDay$x, main = "Number of steps per day", xlab = "Steps per day", ylab = "Frequency", breaks = 8)
```

![](PA1_template_files/figure-html/histandmean-1.png) 

```r
mean(stepsPerDay$x, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(stepsPerDay$x, na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
stepsPerInt = aggregate(ad$steps, list(interval = ad$interval), function(x) {mean(x, na.rm = TRUE)})
plot(stepsPerInt$interval, stepsPerInt$x, type = "l", xlab = "Time interval of day in hhmm", ylab = "Average steps in interval", main = "Average number of steps by time of day (5 min intervals)" )
```

![](PA1_template_files/figure-html/dailypattern-1.png) 

```r
# Print the interval with the highes average dail activity pattern
imax <- which.max(stepsPerInt$x); 
stepsPerInt$interval[imax]
```

```
## [1] 835
```

## Imputing missing values

```r
# Copy data into a new structure
ad2 <- ad
I <- is.na(ad2$steps)
imp <- join(ad2[I,], stepsPerInt, by = c("interval"))
ad2$steps[I] <- imp$x

stepsPerDay2 = aggregate(ad2$steps, list(date = ad2$date), function(x) {sum(x, na.rm = TRUE)})
stepsPerInt2 = aggregate(ad2$steps, list(interval = ad2$interval), function(x) {mean(x, na.rm = TRUE)})

hist(stepsPerDay2$x, main = "Number of steps per day", xlab = "Steps per day", ylab = "Frequency", breaks = 8)
```

![](PA1_template_files/figure-html/impute-1.png) 

```r
mean(stepsPerDay2$x, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay2$x, na.rm = TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
Sys.setlocale("LC_TIME", "en_US")
```

```
## [1] "en_US"
```

```r
isWeekend <- function(x) {
  if(x %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
    r <- "weekday"
  } else if (x %in% c("Saturday", "Sunday")) {
    r <- "weekend"
  } else {
    r <- NA
  }
  r
}

wd <- weekdays(as.Date(ad$date))


ad$weekend <- sapply(wd, isWeekend)

I <- ad$weekend == "weekend"

stepsPerIntWe = aggregate(ad$steps[I], list(interval = ad$interval[I]), function(x) {mean(x, na.rm = TRUE)})
stepsPerIntWd = aggregate(ad$steps[!I], list(interval = ad$interval[!I]), function(x) {mean(x, na.rm = TRUE)})

plot(stepsPerIntWe$interval, stepsPerIntWe$x, type = "l", xlab = "Time interval of day in hhmm, ", ylab = "Average steps in interval", main = "Average number of steps by time of day (5 min intervals) -- Weekends" )
```

![](PA1_template_files/figure-html/differences-1.png) 

```r
plot(stepsPerIntWd$interval, stepsPerIntWd$x, type = "l", xlab = "Time interval of day in hhmm, ", ylab = "Average steps in interval", main = "Average number of steps by time of day (5 min intervals) -- Weekdays" )
```

![](PA1_template_files/figure-html/differences-2.png) 

