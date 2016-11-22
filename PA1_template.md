---
title: "Reproducible Research: Peer Assessment 1"
author: "Lisa Anderson"
date: "November 17, 2016"
output: html_document
---

##Introduction    

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.   
   
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.   
   
The data for this assignment can be downloaded from the course web site:    
    
**Dataset**: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]    
    
The variables included in this dataset are:     
    
**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)    
**date**: The date on which the measurement was taken in YYYY-MM-DD format    
**interval**: Identifier for the 5-minute interval in which measurement was taken    
    
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.       
   
## Loading and preprocessing the data  
  
* Load pacakges   
* Download, unzip, and read the data into a new dataframe    
     

```r
library(plyr)    
library(Hmisc)   
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:plyr':
## 
##     is.discrete, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(ggplot2)   

if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}

data <- read.csv("activity.csv")
```
   
## What is mean total number of steps taken per day?   
1. Make a histogram of the total number of steps taken each day   
2. Calculate and report the mean and median total number of steps taken per day   
   
**Solution** 
   
* Sum the steps by date and create a histogram to show the frequency.   
* Take the mean and the median of the steps per day   


```r
steps_per_day <- aggregate(steps ~ date, data, sum)
with(steps_per_day, hist(steps, main = paste("Total Steps by Day"), col="green", xlab="# of Steps"))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
steps_mean <- mean(steps_per_day$steps)
steps_median <- median(steps_per_day$steps)
```
   
   
**The mean is 1.0766189 &times; 10<sup>4</sup>.**      
**The median is 10765.**    
  

## What is the average daily activity pattern?  
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).    
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?   
    
**Solution**   
   
* Aggregate data to caculate the average steps per interval.   
* Create a time series plot to show the steps per 5-minute interval.       
* Calculate which interval contains the max number of steps on average.    
   

```r
steps_int <- aggregate(steps ~ interval, data, mean)

with(steps_int,plot(interval, steps, type="l", col="purple", xlab="5-Minute Interval", ylab="# of Steps",main="Average Steps per Day by Interval"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
max_int <- steps_int[which.max(steps_int$steps),1]
```
      
**The 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps is interval 835.**   
   
    
## Imputing missing values   
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)   
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.   
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.    
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?   
     
**Solution**   
   
* Calculate sum of NA values    
   
   

```r
incomplete <- sum(!complete.cases(data))   
```
   
**The total number of NA values is 2304**   
    
      
* Create new dataset with no missing values by replacing NA values with the average steps per interval.  

```r
imputed <- ddply(data, "interval", mutate, imputed.value = impute(steps, mean))
imputed$imputed.value <- round(imputed$imputed.value)
imputed <- imputed[c(4,2,3)]
imputed <- setNames(imputed, c("steps","date","interval"))   
```
       
* Sum the steps of the new dataset by date and create a histogram to show the frequency.   
* Take the mean and the median of the steps per day   
   
      

```r
steps_imputed <- aggregate(steps ~ date, imputed, sum)
with(steps_imputed, hist(steps, main = paste("Total Steps by Day (Imputed)"), col="blue", xlab="# of Steps"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
steps_imp_mean <- mean(steps_imputed$steps)
steps_imp_median <- median(steps_imputed$steps)
```
    
**The mean for the imputed dataset is 1.0765639 &times; 10<sup>4</sup>.**      
**The median for the imputed dataset is 1.0762 &times; 10<sup>4</sup>.**    
    
* Calculate the difference between the original and the imputed means and medians.   
* Calculate the total difference in steps between the original and imputed datasets.   

   

```r
mean_diff <- steps_imp_mean - steps_mean   
med_diff <- steps_imp_median - steps_median   
total_diff <- sum(steps_imputed$steps) - sum(steps_per_day$steps)   
```
   
**The difference between the non-imputed mean and imputed mean is -0.549335.**   
**The difference between the non-imputed mean and imputed mean is -3.**      
**The difference between the total number of steps between the 2 datasets is 8.6096 &times; 10<sup>4</sup>.**       
     
   
## Are there differences in activity patterns between weekdays and weekends? 
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.   
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).   
   
**Solution**  
   
* Create a column indicating if the day of the week is a weekday or weekend. 
* Aggregate the data to get the average steps by interval and day.     
* Plot weekday and weekend data to compare the steps by interval.   
   

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed$day = as.factor(ifelse(is.element(weekdays(as.Date(imputed$date)),weekdays), "Weekday", "Weekend"))

steps_interval_imp <- aggregate(steps ~ interval + day, imputed, mean)   

ggplot(steps_interval_imp, aes(x=interval, y=steps)) +       
geom_line(color="blue") + facet_wrap(~ day, nrow=2, ncol=1) +   
labs(x="5-Minute Interval", y="# of Steps") + theme_bw()    
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
   
**According to the above chart, the weekends show more overall activity throughout the day. However, the weekdays have a higher peak in the beginning of the day as compared to weekends.**    
