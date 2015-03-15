# PROGRAMMING ASSIGNMENT 1.  

output:
html_document:
keep_md: true

## Loading and preprocessing the data. 

1. Load the data.
opts_chunk$set(echo=TRUE, results='asis')

```r
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, "./activity.zip", mode="wb")
unzip("./activity.zip")
data<-read.csv("./activity.csv",header=TRUE,sep=",")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis.

```r
library(lubridate)
data$date<-ymd(data$date)
```


## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day.

```r
total<-tapply(data$steps,data$date,sum,na.rm=TRUE)
hist(total,main="Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

2. Calculate and report the mean and median of the total number of steps taken per day.

```r
mean(total)
```

```
## [1] 9354.23
```

```r
median(total)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l" ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
mean<-tapply(data$steps,data$interval,mean,na.rm=TRUE)
int<-strptime(sprintf("%04d",as.numeric(names(mean))),format="%H%M")
plot(int,mean,type="l",xlab="5-minute intervals",ylab="Average number of steps taken",main="Average daily activity")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max<-which.max(mean)
sprintf("%04d",data[max,"interval"],format="%H%M")
```

```
## [1] "0835"
```


## Imputing missing values.
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```r
sum(rowSums(is.na(data))>0)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
impdata<-data
impdata$steps[is.na(data$steps)]=mean(data$steps,na.rm=TRUE)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
imptotal<-tapply(impdata$steps,impdata$date,sum,na.rm=TRUE)
hist(imptotal,main="Total number of steps taken each day (imputed NA's)")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
mean(imptotal)
```

```
## [1] 10766.19
```

```r
median(imptotal)
```

```
## [1] 10766.19
```
We get a normal distribution by imputing missing values. Median and mean values are identical.


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
wd<-weekdays(impdata$date)
wd<-factor(wd)
levels(wd)<-list(weekday=c("lunes","martes","miércoles","jueves","viernes"),weekend=c("sábado","domingo"))
table(wd)
```

```
## wd
## weekday weekend 
##   12960    4608
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(ggplot2)
library(scales)
meanimp<-tapply(impdata$steps,interaction(impdata$interval,wd),mean,na.rm=TRUE)
daydf<-data.frame(meanimp,
                  intimp=unique(impdata$interval),
                  dayimp=as.factor(c(rep("weekday",288),rep("weekend",288))))
ggplot(daydf,aes(intimp,meanimp))+geom_line()+xlab("5-minute intervals") + 
    ylab("Average number of steps taken")+labs(title ="Average daily activity")+
    facet_grid(dayimp~.)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
