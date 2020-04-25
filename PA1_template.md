---
title: "Reproducible data analysis (Project One)"
author: "JYu"
date: "26/04/2020"
output:
  word_document: default
  html_document: default
---

**This is the submission document for Assingment One of the Reproducible Data Ananlysis Course**
================================================================================================

#Part 1: Loading data
##Task one: code to load the data
```{r}
#setting working directory
setwd<- "C:/Users/jiameng.yu/Desktop/Statistics_course/Course 5 - Reproducible Research/Course project 1"
rawData <- read.csv("repdata_data_activity/activity.csv")
head(rawData)
str(rawData)
```


##Task 2: transforming data
###date is saved as Factor. This variable needs to be transformed. 
```{r}
library(dplyr)
processData1<-  mutate(rawData,Date=as.Date(rawData$date,"%Y-%m-%d"))
cleanData <- select(processData1,-date)
str(cleanData)
```

#Part 2: Calculate mean number of steps each day
## Task 1: calculate total number of steps each day
```{r}
  T_steps1<- group_by(cleanData,Date)
  T_steps<- summarise(T_steps1, TStep = sum(steps,na.rm=TRUE))
  head(T_steps)
```



## Task 2: Make a histogram 
```{r}
hist(T_steps$TStep, main="Total steps taken each day")
```


## Task 3: calculate and report the mean and median of total number of steps taken per day
```{r}
Step_mean <- mean(T_steps$TStep)
Step_median <- median(T_steps$TStep)
Step_mean
Step_median
```


# Part 3: Analysing average daily activity pattern
## Task 1: make a time series plot of 5 min interval and average steps taken
```{r}
A_steps1<- group_by(cleanData,interval)
A_steps<- as.data.frame(summarise(A_steps1,AStep = mean(steps,na.rm=TRUE)))
head(A_steps)
plot(A_steps$interval,A_steps$AStep,type="l",main="Time series for averages steps per interval across all days")
```



##Task 2: find the 5-min interval with highest max number of steps
```{r}
Max_steps<- as.data.frame(summarise(A_steps1, MStep = max(steps,na.rm=TRUE)))
M_step<- arrange(Max_steps,desc(MStep))
head(M_step,1)
```
#Part 4: Inputting misssing values
## Task 1: Calculate and report the total number of missing values
```{r}
missingValues<- filter(cleanData,is.na(steps))
nrow(missingValues)
```


## Task2: Filling in missing values (with avearage value for that interval)
```{r}
combinedSteps<- as.data.frame(merge(A_steps,cleanData,by="interval"))
combinedSteps<- combinedSteps %>%
  mutate(newsteps = steps %>% 
             is.na %>%
             ifelse(AStep, steps))
head(combinedSteps)
```



## Task 3: Create a new dataset with missing values filled in
```{r}
newcleanData <- combinedSteps %>%
  mutate(Steps=as.integer(newsteps))
newcleanData<- newcleanData[,c(4,1,6,2)]
head(newcleanData)
```
## Task 4
### Sub-task 1: make a histogram of total number of steps taken per day
```{r}
T_steps2<- group_by(newcleanData,Date)
  T_stepscomplete<- summarise(T_steps2, TStep = sum(Steps,na.rm=TRUE))
  hist(T_stepscomplete$TStep, main="Total steps taken each day (missing data filled)")
```



### Sub-task 2: calculate new mean an median each day
```{r}
Step_meancomplete <- mean(T_stepscomplete$TStep)
Step_mediancomplete <- median(T_stepscomplete$TStep)
Step_meancomplete
Step_mediancomplete
```



### Sub-tasks 3 & 4: do these values differ from results calculated in task 1? 
### What is the impact?
```{r}
delta1<- Step_mean-Step_meancomplete
delta2<- Step_median-Step_mediancomplete
delta1
delta2
#### The mean and median steps per day calculated before  
#### were less those after missing values were replaced.  
#### By replacing missing vlaues, the daily mean and median
#### were increased. 
```



#Part 5: Activity patterns during weekend and weekdays
## Task 1: create a new factor indicating whether a day is weekday or weekend
```{r}
library(lubridate)
newcleanData1<- mutate(newcleanData,Weekday=wday(newcleanData$Date,week_start=1))
weekenddata<- filter(newcleanData1,Weekday>5)
weekdaydata<- filter (newcleanData1, Weekday<6)
weekenddata1<- mutate(weekenddata,WDAY="Weekend")
weekdaydata1<- mutate(weekdaydata,WDAY="Weekday")
newData1<- rbind(weekenddata1,weekdaydata1)
newData<- select(newData1,-Weekday)
head(newData)
str(newData)
```



## Task 2: make a panel plot
```{r}
library(lattice)
xyplot(AStep ~ interval | WDAY, data=newData1 ,type="l", layout=c(1,2),
    main="Average Steps per interval")
```
