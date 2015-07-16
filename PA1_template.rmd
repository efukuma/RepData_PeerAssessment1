---
title: "PA1_template"
output: html_document
---

##Loading and preprocessing the data

Libraries
```{r}
library(dplyr)
library(ggplot2)
```

Load de csv file
```{r}
actData <- read.csv("data/activity.csv")
head(actData)
```

Change de type of date column and interval
```{r}
actData$date <- as.Date(actData$date, "%Y-%m-%d")
```

##What is mean total number of steps taken per day?

Calculating total steps by day
```{r}
tot_steps_day <-
  actData %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) 
```

Histogram with total steps by day
```{r}
hist(tot_steps_day$steps,10, main = "Total steps per day", xlab = "steps")
```

mean and median
```{r}
mean_steps   <- mean(tot_steps_day$steps)
median_steps <- median(tot_steps_day$steps)
```

The mean is `r mean_steps` and the mediam is `r median_steps`

##What is the average daily activity pattern?

Calculating mean steps by interval
```{r}
mean_steps_interval <-
  actData %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps)) 
```
  
graphic with mediam steps by interval of 5 minuts
```{r}
qplot(x=interval, y=steps, data=mean_steps_interval, geom = "line", main="Average Number of Steps")
```

Finding the interval with max steps
```{r}
max_steps_interval <- mean_steps_interval[which.max(mean_steps_interval$steps),]
```
The interval is `r max_steps_interval$interval` with `r max_steps_interval$steps` steps

##Imputing missing values:

Counting NA
```{r}
count_na <- sum(is.na(actData$steps))
```
The total number of na is `r count_na`

Finding NAs
```{r}
pos_na <- which(is.na(actData$steps))
```

calculating mean for all and replacing na values
```{r}
mean_na <- rep(mean(actData$steps, na.rm=TRUE), times=length(pos_na))
actData[pos_na, "steps"] <- mean_na
```

Calculating total steps by day without na
```{r}
tot_steps_day <-
  actData %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) 
```

Histogram with total steps by day
```{r}
hist(tot_steps_day$steps,10, main = "Total steps per day", xlab = "steps")
```

mean and median
```{r}
mean_steps   <- mean(tot_steps_day$steps)
median_steps <- median(tot_steps_day$steps)
```
The mean is `r mean_steps` and the mediam is `r median_steps`

## Are there differences in activity patterns between weekdays and weekends?

Determining weekday and weekend days
(in this case consider sábado = saturday and domingo = sunday)
```{r}
type_day <- function(x) {
    if (weekdays(x) %in% c("sábado","domingo")) {
        "weekend"
    } else {
        "weekday"
    }
}
actData$type_day <- as.factor(sapply(actData$date, type_day))
```


```{r}
mean_steps <-
  actData %>%
  group_by(interval,type_day) %>%
  summarize(steps = mean(steps)) 
```

Comparing weekend and weekday
```{r}
ggplot(mean_steps, aes(x = interval, y = steps)) +  geom_line() + facet_grid(type_day~.)
```

