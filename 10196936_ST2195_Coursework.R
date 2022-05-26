# ========================================================================================================
# Purpose:      ST2195 Programming for Data Science Coursework
# Name:         Dillon Yew (10196936)
# Task:         Answer questions related to airline on time data
# Dataset:      2005.csv, 2006.csv, airports.csv, carriers.csv, plane-data.csv
#=========================================================================================================

# Set working directory
setwd("C:/Users/wokie/ST 2195_Programming for Data Science/Coursework")

## Importing libraries -----------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(Hmisc)
library(caret) 
library(rpart) 
library(rpart.plot) 
library(MLmetrics)
library(ggpubr)
library(data.table)


## Data Wrangling-----------------------------------------------------------------------------------------

# Importing datasets
airport <- read.csv("airports.csv")
carrier <- read.csv("carriers.csv")
planes_data <- read.csv("plane-data.csv")
df_2005 <- read.csv("2005.csv")
df_2006 <- read.csv("2006.csv")

# Combining 2005 and 2006 data
df <- rbind(df_2005, df_2006)
summary(df)  

# Checking for Null values
sum(is.na(airport)) 
sum(is.na(carrier)) 
sum(is.na(planes_data))
sum(is.na(df))

# Impute NA values in the dataset with their mean
impute_mean <- apply(df, 2, mean, na.rm=TRUE) 

for(x in colnames(df))
  df[,x][is.na(df[,x])] <- impute_mean[x]


### Q1 to Q5----------------------------------------------------------------------------------------------

## Q1. When is the best time of day, day of the week, and time of year to fly to minimise delays?

# Make a copy of main dataframe for Q1
df1 <- copy(df)

for(x in colnames(df1))
  df[,x][is.na(df1[,x])] <- impute_mean[x]

# Convert Scheduled Departure time into hours format
df1$CRSDepTime <- sprintf("%04d", df1$CRSDepTime)
format(strptime(df1$CRSDepTime, format="%H%M"), format = "%H:%M")
CRSDepHour <- as.numeric(substr(df1$CRSDepTime, start = 1, stop = 2))
df1$CRSDepHour <- CRSDepHour

# Q1a. Best time of the day to fly to minimise delays
df1_time = df1 %>% group_by(CRSDepHour) %>% summarise(DepDelay_Mean = mean(as.numeric(DepDelay),na.rm=TRUE), 
                                                    ArrDelay_Mean = mean(as.numeric(ArrDelay),na.rm=TRUE))

time1 <- ggplot(df1_time, aes(x = CRSDepHour, y = DepDelay_Mean)) +
  geom_bar(stat = 'identity', fill='#1f77b4') +
  theme_classic()  
time2 <- ggplot(df1_time, aes(x = CRSDepHour, y = ArrDelay_Mean)) +
  geom_bar(stat = 'identity', fill='#ff7f0e') +
  theme_classic()  
plot_grid(time1, time2, labels = "AUTO")
# Q1a.Ans: Best time of the day to fly is from 5am to 6am.


# Q1b. Best day of the week to fly to minimise delays
df1_week = df1 %>% group_by(DayOfWeek) %>% summarise(DepDelay_Mean = mean(as.numeric(DepDelay),na.rm=TRUE), 
                                                   ArrDelay_Mean = mean(as.numeric(ArrDelay),na.rm=TRUE))

week1 <- ggplot(df1_week, aes(x = DayOfWeek, y = DepDelay_Mean)) +
  geom_bar(stat = 'identity', fill='#1f77b4') +
  theme_classic()  
week2 <- ggplot(df1_week, aes(x = DayOfWeek, y = ArrDelay_Mean)) +
  geom_bar(stat = 'identity', fill='#ff7f0e') +
  theme_classic()  
plot_grid(week1, week2, labels = "AUTO")
# Q1b.Ans: Best day of the week to fly is on a Saturday


# Q1c. Best time of the year to fly to minimise delays
df1_month = df1 %>% group_by(Month) %>% summarise(DepDelay_Mean = mean(as.numeric(DepDelay),na.rm=TRUE), 
                                                ArrDelay_Mean = mean(as.numeric(ArrDelay),na.rm=TRUE))

month1 <- ggplot(df1_month, aes(x = Month, y = DepDelay_Mean)) +
  geom_bar(stat = 'identity', fill='#1f77b4') +
  theme_classic()  
month2 <- ggplot(df1_month, aes(x = Month, y = ArrDelay_Mean)) +
  geom_bar(stat = 'identity', fill='#ff7f0e') +
  theme_classic()  
plot_grid(month1, month2, labels = "AUTO")
# Q1c.Ans: Best time of the year to fly is in April

# Q1.Ans: Hence, best time to fly to minimise delay is in the month of April, on a Saturday, anytime between 5 am and 6am.

-----------------------------------------------------------------------------------------------------------------------------

## Q2. Do older planes suffer more delays?-----------------------------------------------------------------------------------

# Check for Null values in planes dataset
sum(is.na(planes_data))

#Renaming the column name of plane_data.csv
colnames(planes_data)
names(planes_data)[names(planes_data) == "tailnum"] <- "TailNum"
names(planes_data)[names(planes_data) == "year"] <- "YearOfManufacture"

# Merging df with planes_data
df_planes <- left_join(df, planes_data)
df_planes <- df_planes[-1,]

# Visualization of Departure Delay & Arrival Delay over the Year of Manufacture for the planes
ggplot(df_planes, aes(x=YearOfManufacture, fill=DepDelay, color=ArrDelay)) +
  geom_histogram(stat='count', fill='lightblue', position="identity") 

# Q2.Ans: Majority of delays seems to occur in planes that were built from 1998 to 2004. Hence, older planes do not suffer from more delays.

-----------------------------------------------------------------------------------------------------------------------------------------------

## Q3. How does the number of people flying between different locations change over time?------------------------------------------------------

# Subset datetime variables, Origin and Destination locations
df3 = select(df, Year, Month, DayofMonth, DayOfWeek, Origin, Dest)

# Finding the count of people travelling from the city of Boston(BOS) to Chicago(ORD)
df3_location <- df3[df3$Origin == 'BOS' & 
                      df3$Dest == 'ORD', ]

# Visualization of people travelling from "BOS" to "ORD" over 4 different time-related variables
location1 <-ggplot(df3_location,aes(Year))+geom_line(stat="bin", aes(fill=..count..), colour='violet', binwidth=1)
location2 <-ggplot(df3_location,aes(Month))+geom_line(stat="bin", aes(fill=..count..), colour='violet', binwidth=1)
location3 <-ggplot(df3_location,aes(DayofMonth))+geom_line(stat="bin", aes(fill=..count..), colour='violet', binwidth=1)
location4 <-ggplot(df3_location,aes(DayOfWeek))+geom_line(stat="bin", aes(fill=..count..), colour='violet', binwidth=1)
plot_grid(location1, location2, location3, location4, labels = "AUTO")

# Q3.Ans: 
# The number of people travelling from BOS to ORD decreased from 2005 to 2006. 
# The number of people traveling from BOS to ORD is at its lowest in the month of February while at its peak in October
# The number of people travelling from BOS to ORD is rather consistent from up till the 25th of the month whereby there is a sharp decrease.
# The number of people travelling from BOS to ORD is higher during the weekdays compared to the weekend. Saturday has the least number of people doing so.

--------------------------------------------------------------------------------------------------------------

## Q4 - Can you detect cascading failures as delays in one airport create delays in others?

# Make a copy of main dataframe for Q1
df4 <- copy(df)

# Adding date column to the dataset
toDate <- function(year, month, day) {
  ISOdate(year, month, day)
}
Dateformat <- toDate(df4$Year, df4$Month, df4$DayofMonth)

df4$Date <- Dateformat

# Subsetting columns required to test for cascading failures
df4_data <- select(df4, Date, UniqueCarrier, TailNum, Origin, Dest, Distance, 
                     CRSDepTime, DepTime, DepDelay, CRSArrTime, ArrTime, ArrDelay)
df4_analysis <- df4_data[df4_data$ArrDelay > 0 & df4_data$DepDelay > 0, ]

# Subsetting df4_analysis to only show data for planes with 'TailNum' == 'N326UA'
df4_analysisresults1 <- df4_analysis[df4_analysis$TailNum ==  'N326UA', ]
df4_analysisresults1 <- na.omit(df4_analysisresults)
head(df4_analysisresults1,2)

## Analysis from "df4_analysisresults1" regarding Flight "N326UA" traveling from 'BOS' to 'ORD' on 2005-01-09
# On 2005-01-09, Flight "N326UA" was scheduled to travel from 'BOS' to 'ORD'. 
# Flight "N326UA" was scheduled to depart 'BOS' at 1705 (5.05 pm) but the actual departure time was at 1853 (6.53 pm). There was a departure delay of 108 minutes. 
# Flight "N326UA" was scheduled to arrive at 'ORD' at 1902 (7.02 pm) but the actual arrival time was at 2138 (9.38 pm). There was an arrival delay of 156 minutes. 


# Checking 'ORD' on 2005-01-09 to see if there were any delays
df4_analysisresults2 <- df4_analysis[df4_analysis$TailNum ==  'N326UA'& df4_analysis$Origin == "ORD", ]
df4_analysisresults2 <- na.omit(df4_analysisresults2)
head(df4_analysisresults2,2)

## Analysis from "df4_analysisresults2" regarding data from 'ORD' airport on 2005-01-09
# At 'ORD' airport on 2005-01-09, Flight "N326UA" that was scheduled to depart from 'ORD' to 'SAT' was delayed by 135 minutes. The scheduled departure time was 1950 (7.50 pm) but the actual departure time was 2205 (10.05 pm).
# Hence, we can conclude that the departure delay in the 'BOS' airport on 2005-01-09 caused a departure delay in the 'ORD' airport on the same date.
# Therefore, we conclude that delays in one airport creates delays in other airports. From this dataset, we are able to detect cascading failures where delays in one airport creates delays in others.

# Q4.Ans: We were able to detect cascading failures for flight 'N326UA' on 2005-01-29 as it traveled from 'BOS' to 'ORD'.

----------------------------------------------------------------------------------------------------------------

## Q5 - Use the available variables to construct a model that predicts delays.

# Selecting Variables to test for our prediction model
df5 = select(df,Year,DayofMonth,DepTime,CRSDepTime,DepDelay,
             Distance,TaxiIn,TaxiOut,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay)

# Subsetting model dataframe to only contain 10000 rows
df5_model = head(df5,10000)

# Impute Null values in the dataset with their mean
df5_model$DepTime[is.na(df5_model$DepTime)] <- mean(df5_model$DepTime, na.rm = T)
df5_model$DepDelay[is.na(df5_model$DepDelay)] <- mean(df5_model$DepDelay, na.rm = T)


# Setting up binary column "DepartureDelay"
df5_model$DepartureDelay = 0
df5_model$DepartureDelay[df5_model$DepDelay > 0] <- 1


# Train-Test Split
set.seed(20)
train=sample(1:nrow(df5_model), nrow(df5_model)*0.7)
trainset=df5_model[train,]
testset=df5_model[-train,]


# Logistic Regression Testing
LR_model <- glm(DepartureDelay ~ ., data=df5_model, family="binomial", subset=train)
summary(LR_model)
LR.prob=predict(LR_model, newdata=testset, type="response")
LR.pred=as.factor(ifelse(LR.prob > 0.5, "1", "0"))
confusionMatrix(LR.pred,as.factor(testset$DepartureDelay))

# Q5.Ans: High Accuracy score of close to 100% suggest that the model is highly accurate in predicting flight delays.
# However this could be due to overfitting or variables selected being highly correlated.
