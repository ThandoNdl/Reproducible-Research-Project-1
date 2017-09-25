---
title: "Activity Monitoring"
author: "Thando Ndlovu"
date: "25 September 2017"
output: html_document
---

## Introduction

It is now possible to collect data about personal movement using activity trackers.  

This document looks at data collected from a personal monitoring device which consists of data from an anonymous individual collected in October and November 2012 and include the number of steps taken in 5 minute intervals each day.  

```{r import, echo=FALSE}
rm(list = ls())
setwd("~/RWork/Coursera/Course 5/Week 2/repdata_data_activity")
activity <- read.csv('activity.csv', header = TRUE, sep = ",")
# Remove missiing values
activity2 <- activity[complete.cases(activity),]
```


## Analysis

#### Total Number of Steps Taken Each Day

The following histogram shows the total number of steps taken each day.  
NB: Missing values were excluded from the data for this part;  
Resulting set called "activity2".

```{r histogram, echo = TRUE}

totActivity_day <- aggregate(activity2$steps, by = list(day = activity2$date), FUN=sum)

hist(totActivity_day$x, main = "Histogram for Number of Steps taken per day",
      xlab = "Number of steps", breaks = 50, col = "light blue")

```

```{r mean_med, echo = FALSE}
meanActivity <- mean(totActivity_day$x)
medianActivity <- median(totActivity_day$x)
```

The mean total number of steps taken per day:            `r meanActivity`  
The median total number of steps taken per day:          `r medianActivity`  

#### The Average Daily Activity Pattern

The following time series plot shows the average number of steps taken per 5-minute interval averaged across all days.

```{r time_series, echo = TRUE}
totActivity_int <- aggregate(activity2$steps,
                      by = list(interval = activity2$interval), FUN = mean)
plot(totActivity_int$interval, totActivity_int$x, type = "l",
                xlab = "5-minute Interval", ylab = "Average number of steps",
              main = "Average Number of Steps per 5 minute interval", col = "purple")
```

```{r max_interval, echo=FALSE}
maxSteps <- totActivity_int[which.max(totActivity_int$x),]
max_last <- as.numeric(maxSteps$interval)
max_first <- max_last - 5
```

The maximum number of steps was in the `r max_first` - `r max_last` 5-minute interval.  

#### Imputing Missing Values

```{r num_na, echo = FALSE}
sumNA <- sum(is.na(activity))
```

Unfortunately, this set had `r sumNA` missing values.  

To help with this, I decided to replace all missing values with the average of the corresponding 5-minute interval.  

```{r replace_nas, echo = TRUE, warning=FALSE}

library(magrittr)
library(dplyr)

activityB <- activity %>% group_by(interval) %>% mutate(avg = mean(steps, na.rm = TRUE))
activityB$steps[is.na(activityB$steps)] <- activityB$avg[is.na(activityB$steps)]
activityB$steps2 <- round(activityB$steps, digits = 0)

sum_NA2 <- sum(is.na(activityB))
```

After imputing, we now have `r sum_NA2` missing values!  
  
We're going to relook at the total number of steps taken each day now that we have dealt with the missing values.  

```{r histogram2, echo = TRUE}

totActivity_day_noNA <- aggregate(activityB$steps2, by = list(day = activityB$date), FUN=sum)

hist(totActivity_day_noNA$x, main = "Histogram for Number of Steps taken per day",
     xlab = "Number of steps", breaks = 50, col = "dark blue")

```

Did imputing values change the distribution of our data?  

```{r mean_med2, echo = FALSE}
meanActivityB <- mean(totActivity_day_noNA$x)
medianActivityB <- median(totActivity_day_noNA$x)
```

The mean total number of steps taken per day with missing values:  
      `r meanActivity`  
The mean total number of steps taken per day without missing values:  
      `r meanActivityB`  
The median total number of steps taken per day with missing values:  
      `r medianActivity`  
The median total number of steps taken per day without missing values:  
      `r medianActivityB`  
        
The values are quite close to each other, so we can assume it's safe to use the set with imputed values than the set with missing values.  

#### Activity Patterns between weekdays and  weekends

We will first by classifying each date according to the day of the week it was on and then group it further to whether it was a weekday or weekend.  

```{r day_split, echo = TRUE}
weekdays1 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
activityC <- activityB %>% mutate(type = weekdays(as.Date(date)))
activityC$type2 <- factor(activityC$type %in% weekdays1, levels = c(TRUE, FALSE),
                          labels = c("Weekday", "Weekend"))

```

The following time series plots show the average number of steps taken per 5-minute interval averaged across all days, divided into weekdays and weekends.  

```{r time_series2, echo = TRUE}

activityC2 <- activityC %>% group_by(interval, type2) %>% mutate(avg = mean(steps))

library(ggplot2)
g <- ggplot(data = activityC2, aes(x = interval, y = avg, color = type2),
              ylab = "Average Number of Steps", xlab = "5 minute interval")
g + geom_line() + facet_grid( facets = type2 ~ .) +
      xlab("5-minute interval") + ylab("Average Number of Steps") +
      labs(title = "Average Number of Steps per 5 minute interval", color = "Day Type")

```


That is the end of the analysis for now!