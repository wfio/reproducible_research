Reproducible Research - Project 1
=================================
Establish global options and libraries

```r
library(knitr)
time <- format(Sys.time(), "%a %b %d %X %Y")
opts_chunk$set(echo=TRUE, results = "show", warning = FALSE, message = FALSE)
```

```r
library(dplyr)
library(ggplot2)
library(lubridate)
```


```r
#check to see if the file exists and if not then get it
if(!file.exists("activity.csv")){ 
    tempdata<-tempfile() 
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", tempdata) 
    activity_data <- unzip(tempdata) 
    unlink(tempdata) 
} else 
    if(file.exists("activity.csv")){
        df1 <- read.csv("activity.csv", na.strings = c('NA', '<NA>'))
    } else stop("You haven't followed the instructions and extracted the raw data and the code
                  to the same working directory. Please start over.")
```


```r
dfg <- group_by(df1, date) 
dfs <- summarise(dfg, total=sum(steps, na.rm = TRUE)) 
hist(dfs$total,breaks = 4, main = "Activity - Steps in a Day",
     ylab = "Frequency", xlab = "Total Steps per Day")
```

![plot of chunk What is mean total number of steps taken per day](figure/What is mean total number of steps taken per day-1.png) 

```r
dfs.2 <- summarise(dfs, avg=mean(total, na.rm = TRUE), med=median(total))
print(dfs.2)
```

```
## Source: local data frame [1 x 2]
## 
##       avg   med
## 1 9354.23 10395
```


```r
dfg.1 <- group_by(dfg, interval)
dfs.3 <- summarise(dfg.1, avg=mean(steps, na.rm = TRUE)) 
plot(dfs.3$interval, dfs.3$avg, type = 'l', main = "Average Daily Activity ",
     xlab = "Interval (t = 5 min)", ylab = "Average Steps per Day")
```

![plot of chunk What is the average daily activity pattern](figure/What is the average daily activity pattern-1.png) 


```r
df1.1 <- group_by(df1, date)
df1.2 <- summarise(df1.1, avg = mean(steps, na.rm = TRUE))
df1.3 <- merge(df1, df1.2, all.x = TRUE)
df1.4 <- mutate(df1.3, steps = ifelse(is.na(steps)==TRUE, avg, steps))
df1.5 <- group_by(df1.4, date)
df1.6 <- summarise(df1.5, total=sum(steps, na.rm = TRUE)) 
hist(df1.6$total,breaks = 4, main = "Activity - Steps in a Day (Imputed Data)",
     ylab = "Frequency", xlab = "Total [Imputed] Steps per Day")
```

![plot of chunk Imputing missing values](figure/Imputing missing values-1.png) 

```r
dfs2.1 <- summarise(df1.6, avg=mean(total, na.rm = TRUE), med=median(total))
```

```r
dayType <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday') 
df1.4$dayType <- factor((ifelse(weekdays(as.Date(df1.4$date)) %in% dayType,'weekday', 'weekend'))) 
df1.4.grouped <- group_by(df1.4, interval, dayType) 
df1.4.summarised <- summarise(df1.4.grouped, avgInt = mean(steps, na.rm = TRUE))
qplot(interval, avgInt, data = df1.4.summarised, geom = 'line', facets = dayType ~ ., 
      main = "Weekday vs Weekend Steps - A Time Series Plot", xlab = "Interval ((t) = 5min)",
      ylab = "Number of Steps")
```

![plot of chunk Weekend vs Weeday Steps - Time Series](figure/Weekend vs Weeday Steps - Time Series-1.png) 

This HTML markdown file was generated: Sun Aug 16 03:43:04 PM 2015.
