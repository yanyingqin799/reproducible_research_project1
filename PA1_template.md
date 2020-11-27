Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

1.Loading and preprocessing the data
 knitr::opts_chunk$set(echo = TRUE)
1
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = temp )
unzip(temp, "activity.csv")
activity<-read.csv("activity.csv")
head(activity)

What is mean total number of steps taken per day?
stepsum<-tapply(activity$steps,activity$date,sum)
hist(stepsum)

2.the mean
mean(stepsum,na.rm = TRUE)

3.the median
median(stepsum,na.rm = TRUE)

4.What is the average daily activity pattern?
stepave<-aggregate(steps~interval,data = activity,FUN = mean,na.rm=TRUE)
plot(stepave$interval,stepave$steps)

5.which max
stepave$interval[which.max(stepave$steps)]

6.Imputing missing values
cleardate<-activity
addmiss<-function(mystep,myinterval){
  if(is.na(mystep))
      newvalue<-stepave$steps[which(stepave$interval==myinterval)]
  else
      newvalue<-mystep
  return(newvalue)
}
cleardate$steps<-mapply(addmiss,cleardate$steps,cleardate$interval)
summary(cleardate)

7.show the new hist
hist(tapply(cleardate$steps,cleardate$date,sum))

8.Are there differences in activity patterns between weekdays and weekends?
cleardate$date<-as.Date(cleardate$date)
library(ggplot2)
isweekday<-function(idate){
   if (weekdays(idate) %in% c("星期日", "星期六"))
       return("weekend")
   else
       return("weekday")
}
cleardate$daytype<-mapply(isweekday,cleardate$date)
ggplot(cleardate)+geom_point(aes(x=interval,y=steps,colour=daytype))
