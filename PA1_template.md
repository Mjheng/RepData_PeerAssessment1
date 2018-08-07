##  Loading and preprocessing the data

we will load and process data.


```r
data1<-read.csv('activity.csv',sep = ',')
```

##  What is mean total number of steps taken per day?

- Calculate the total number of steps taken per day


```r
sapply(split(data1$steps,data1$date),sum,na.rm=TRUE)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

- Make a histogram of the total number of steps taken each day

```r
hist(sapply(split(data1$steps,data1$date),sum,na.rm=TRUE),xlab="number of steps",main = "Histogram of total steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

- Calculate and report the mean and median of the total number of steps taken per day

```r
mean(sapply(split(data1$steps,data1$date),sum,na.rm=TRUE))
```

```
## [1] 9354.23
```

```r
median(sapply(split(data1$steps,data1$date),sum,na.rm=TRUE))
```

```
## [1] 10395
```

##  What is the average daily activity pattern?

- Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
data2<-sapply(split(data1$steps,as.factor(data1$interval)),mean,na.rm=TRUE)
data3<-data.frame(interval=as.integer(names(data2)),averagestep=data2)
plot(data3$interval,data3$averagestep,type="l",xlab='interval',ylab = 'average steps')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

- Find the 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
data3[which(data3$averagestep==max(data3$averagestep)),1]
```

```
## [1] 835
```
##  Imputing missing values

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 
NAs)

```r
sum(is.na(data1$steps))
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. We use the mean for that 5-minute interval and we also create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data4<-data.frame(steps = rep(0,nrow(data1)),date=rep(0,nrow(data1)),interval = rep(0,nrow(data1)))
for (i in seq(1,nrow(data1))){
  if(is.na(data1$steps)[i]){
    data4$steps[i]<-data3[which(data3$interval==data1$interval[i]),2]
  }else{
    data4$steps[i]<-data1$steps[i]
  }
}
data4$interval<-data1$interval
data4$date<-data1$date
```

- Make a histogram of the total number of steps taken each day

```r
hist(sapply(split(data4$steps,data4$date),sum,na.rm=TRUE),xlab="number of steps",main = "Histogram of total steps per day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

- Calculate and report the mean and median total number of steps taken per day. 

```r
mean(sapply(split(data4$steps,data4$date),sum,na.rm=TRUE))
```

```
## [1] 10766.19
```

```r
median(sapply(split(data4$steps,data4$date),sum,na.rm=TRUE))
```

```
## [1] 10766.19
```

- The impact of imputing missing data on the estimates of the total daily number of steps.

```r
data.frame(type=c('mean1','mean2','median1','median2'),number=c(mean(sapply(split(data1$steps,data1$date),sum,na.rm=TRUE)),mean(sapply(split(data4$steps,data4$date),sum,na.rm=TRUE)),median(sapply(split(data1$steps,data1$date),sum,na.rm=TRUE)),median(sapply(split(data4$steps,data4$date),sum,na.rm=TRUE))))
```

```
##      type   number
## 1   mean1  9354.23
## 2   mean2 10766.19
## 3 median1 10395.00
## 4 median2 10766.19
```
We can tell from above that the mean and median increase after imputing missing data.

##  Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data4$type<-as.character(rep(0,nrow(data4)))
daytime<-weekdays(as.Date(data4$date))
for (i in seq(1,nrow(data4))){
  daytime[i]<-ceiling(i/288)
}
daytime<-as.integer(daytime)
for (i in seq(1,nrow(data4))){
  if(daytime[i] %in% c(1,2,3,4,5)){
    data4$type[i]<-'weekday'
  }else{
    data4$type[i]<-'weekend day'
  }
}
```

- Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
data5<-subset(data4,data4$type=='weekday')
data6<-subset(data4,data4$type=='weekend day')
data7<-sapply(split(data5$steps,as.factor(data5$interval)),mean,na.rm=TRUE)
data8<-sapply(split(data6$steps,as.factor(data6$interval)),mean,na.rm=TRUE)
data9<-data.frame(interval=as.integer(names(data7)),averagestep=data7)
data10<-data.frame(interval=as.integer(names(data8)),averagestep=data8)
par(mfrow=c(2,1),mar=c(4,4,1,1))
plot(data9$interval,data9$averagestep,type="l",main='weekday',xlab='interval',ylab = 'Number of steps')
plot(data10$interval,data10$averagestep,type="l",main='weekend day',xlab='interval',ylab = 'Number of steps')
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
