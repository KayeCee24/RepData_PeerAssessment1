---
title: 'Reproducible Research: Peer Assessment 1'
output: html_document
keep_md: true
---

library("ggplot2")
library("dplyr")

# Loading and preprocessing the data. Reading csv Data into Data.Table
```{r loaddata}
if(!file.exists("./data")) {dir.create("./data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

download.file(fileUrl,destfile="./data/activity.zip")

unzip(zipfile="./data/activity.zip",exdir="./data")

ActivityData1 <- data.table::fread(input = "activity.csv")
```

# What is mean total number of steps taken per day?
```{r}
TotDailySteps1 <- ActivityData1 %>% group_by(date) %>% 
  summarize(sumsteps = sum(steps, na.rm = TRUE)) 

head(TotDailySteps1,10)

hist(TotDailySteps1$sumsteps, main = "Histogram of Daily Steps", 
     col = "blue", xlab = "Steps", ylim = c(0,30))

Mean1 <- round(mean(TotDailySteps1$sumsteps),digits = 2)
print(Mean1)

Median1 <- round(median(TotDailySteps1$sumsteps),digits = 2)
print(Median1)
```

# What is the average daily activity pattern?
```{r}
TimeSeries1 <- ActivityData1 %>% group_by(interval) %>%
  summarize(meansteps = mean(steps, na.rm = TRUE)) 

plot(TimeSeries1$meansteps ~ TimeSeries1$interval,
     col = "blue", type = "l", xlab = "5-Minute Intervals", ylab = "Average Steps/Day",
     main = "Average Daily Steps")
```

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
print(TimeSeries1$interval[which.max(TimeSeries1$meansteps)])

print(round(max(TimeSeries1$meansteps),digits=2))
```

# Imputing missing values
```{r MissingData}
print(sum(is.na(ActivityData1$steps)))
```

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
ActivityData2 <- ActivityData1  
for (i in 1: nrow(ActivityData1)) {
  if(is.na(ActivityData1$steps[i])) {
    ActivityData2$steps[i] <- TimeSeries1$meansteps[ActivityData2$interval[i] == 
                                                      TimeSeries1$interval]
  }
}

head(ActivityData2, 10)
```

# Make a histogram...
```{r}
TotDailySteps2 <- ActivityData2 %>% group_by(date) %>%
  summarize(sumsteps = sum(steps, na.rm = TRUE))

hist(TotDailySteps2$sumsteps, main = "Histogram of Daily Steps", 
     col = "blue", xlab = "Steps")

Mean2 <- round(mean(TotDailySteps2$sumsteps), digits = 2)

print(Mean2)

Median2 <- round(median(TotDailySteps2$sumsteps), digits = 2)

print(Median2)

MeanMedian <- data.frame(mean = c(Mean1,Mean2), median = c(Median1,Median2))

rownames(MeanMedian) <- c("1", "2")

print(MeanMedian)
```

# Are there differences in activity patterns between weekdays and weekends?
```{r}
ActivityData3 <- ActivityData2

ActivityData3$date <- as.Date(ActivityData3$date)

ActivityData3$day <- ifelse(weekdays(ActivityData3$date) %in% 
                              c("Saturday", "Sunday"), "Weekend", "Weekday")

ActivityData3$day <- as.factor(ActivityData3$day)
```

# Make a panel plot containing a time series plots of average number of steps taken on weekdays and weekends.
```{r}
WkdayData <- filter(ActivityData3, ActivityData3$day == "Weekday")

WkendData <- filter(ActivityData3, ActivityData3$day == "Weekend")

WkdayData <- WkdayData %>% group_by(interval) %>%
  summarize(steps = mean(steps))

WkdayData$day <- "Weekday"

WkendData <- WkendData %>% group_by(interval) %>%
  summarize(steps = mean(steps))

WkendData$day <- "Weekend"

WeekType <- rbind(WkdayData, WkendData)

WeekType$day <- as.factor(WeekType$day)

Plot <- ggplot(WeekType, aes (interval, steps))

Plot + geom_line() + facet_grid (day ~ .) + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) + 
  labs(y = "No. of Steps") + labs(x = "Interval") + 
  ggtitle("Average Number of Steps (WeekType)") + 
  theme(plot.title = element_text(hjust = 0.5))
```
