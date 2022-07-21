# ========================================================================================================
# Purpose:      ST2195 Programming for Data Science Coursework
# Name:         Dillon Yew (10196936)
# Task:         Answer questions related to airline on time data
# Dataset:      2005.csv, 2006.csv, airports.csv, carriers.csv, plane-data.csv
#=========================================================================================================

## Importing libraries ---------------------------------------------------------

library(Hmisc)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(plotROC) 
library(pROC) 
library(caret) 
library(PRROC) 
library(rpart) 
library(rpart.plot) 
library(ranger) 
library(MLmetrics)
library(ggpubr)
library(tidyverse)
options(warn = -1)

## Data Wrangling---------------------------------------------------------------

# Set working directory
setwd("C:/Users/wokie/ST 2195_Programming for Data Science/Coursework")

# Import datasets
airport <- read.csv("airports.csv")
carrier <- read.csv("carriers.csv")
planes_data <- read.csv("plane-data.csv")
df_2005 <- read.csv("2005.csv")
df_2006 <- read.csv("2006.csv")

# Combining 2005 and 2006 data
df <- rbind(df_2005, df_2006)

# Examining dataset
summary(df)

#Imputing NULL values in the dataset
# getting median of each column using apply() 
all_column_median <- apply(df, 2, mean, na.rm=TRUE)

for(i in colnames(df))
  df[,i][is.na(df[,i])] <- all_column_median[i]

#--------------------------------------------------------------------------------------------------------------

#### Q1. When is the best time of day, day of the week, and time of year to fly to minimise delays?

tbl <- with(df, table(UniqueCarrier,Month))
barplot(tbl, beside = TRUE, legend = TRUE)

#Plotting graph for CancellationCode and Month
ggplot(data = df, aes(x = factor(df$DayofMonth), y = df$CancellationCode)) +       
  geom_line()

#Checking delay on Week Level
df_week = df %>% group_by(DayOfWeek) %>% summarise(ArrMeanDelay = mean(as.numeric(ArrDelay),na.rm=TRUE), 
                                                   DepMeanDelay = mean(as.numeric(DepDelay),na.rm=TRUE))

plot(df_week$ArrMeanDelay,type = "o",col = "black", xlab = "Arrival Mean", ylab = "Day of the Week", 
     main = "Arrival Delay per week")
lines(df_week$DepMeanDelay, type = "o", col = "gray")


#Q1a Answer - Looking at the Graph Saturday is good time to avoid delays


#Checking delay on Day of month Level
df_monthday = df %>% group_by(DayofMonth) %>% summarise(ArrMeanDelay = mean(as.numeric(ArrDelay),na.rm=TRUE), 
                                                   DepMeanDelay = mean(as.numeric(DepDelay),na.rm=TRUE))

ggplot(df_monthday, aes(x = DayofMonth, y = ArrMeanDelay)) +
  geom_bar(stat = 'identity') +
  theme_classic()

#Q1b - Looking at the graph it seems like 4th of the month is the best way to avoid delay



#Checking delay on Month Level
df_month = df %>% group_by(Month) %>% summarise(ArrMeanDelay = mean(as.numeric(ArrDelay),na.rm=TRUE), 
                                                     DepMeanDelay = mean(as.numeric(DepDelay),na.rm=TRUE))

ggplot(df_month, aes(x = Month, y = ArrMeanDelay)) +
  geom_bar(stat = 'identity') +
  theme_classic()  


#Q1c Anwer - Looking at the graphs it seems like April is the  best month to avoid delay

#--------------------------------------------------------------------------------------------------------------

#Q2 Do older planes suffer more delays?

#Renaming the colnumn name of plane_data.csv

colnames(planes_data)[1] <- "TailNum"
colnames(planes_data)[9] <- "pln_mfg_year"

df_plane_data = merge(planes_data, df, by = "TailNum")
df_plane_data = df_plane_data[-1,]
#Plotting histogram for plane mfg year
ggplot(df_plane_data, aes(x=pln_mfg_year, fill=ArrDelay, color=DepDelay)) +
  geom_histogram(position="identity",stat = 'count')

#Q2 Answer - Looking at the graph it seems like older planes dont suffer much delays.


#--------------------------------------------------------------------------------------------------------------

# Q3 - How does the number of people flying between different locations change over time? 

#Taking subset of columns from original dataframe
df_location = select(df, Year,Month,DayofMonth,DayOfWeek,Origin,Dest,Distance)

#Getting row for a specific locations to look into
df_loc = df_location[df_location$Origin == 'BOS' & df_location$Dest == 'ORD', ]


#PLotting line chart to show passenger travel on monthwise
ggplot(df_loc,aes(Month))+geom_line(aes(fill=..count..),stat="bin",binwidth=1)


#PLotting line chart to show passenger travel on Day of month
ggplot(df_loc,aes(DayofMonth))+geom_line(aes(fill=..count..),stat="bin",binwidth=1)

#PLotting line chart to show passenger travel on days of week
ggplot(df_loc,aes(DayOfWeek))+geom_line(aes(fill=..count..),stat="bin",binwidth=1)


#Q3 - Answer - Looking at the above graphs, it seems like people travel pattern from BOS to ORD is less at the end of the month, and on Saturdays and  in May-June month.

#--------------------------------------------------------------------------------------------------------------


# Q4 - Can you detect cascading failures as delays in one airport create delays in others?

#Subsetting required columns
df_analysis = select(df, DayofMonth,UniqueCarrier,TailNum,Origin,Dest,Distance,CRSDepTime,DepTime,DepDelay,CRSArrTime,ArrTime,ArrDelay)
print(head(df_analysis))


#Getting all records where DepDelay and ArrDelay > 0
rslt_df = df_analysis[df_analysis$DepDelay > 0 & df_analysis$ArrDelay > 0, ]


# Q4 - Anwer: Yes, there are cascading failures as delays in one airport create delays in others. For example:
#Explanation : Flight ‘N902UA’ has to depart at 1554 (3.54 PM) ON 11 of the MONTH; but it is departed at 16:58 (4.58 PM). It is ~1 Hr delayed. It is flew from DFW to ORD . Now it supposed to reached at 22:23 (10.23 PM), but reached at 22:59 ( 10.59 PM).
rslt = df_analysis[df_analysis$DayofMonth == 11 & df_analysis$TailNum ==  'N902UA', ]
head(rslt,1)


##Now let see what happened at ORD airport at 11 of the MONTH?
rslt_df = df_analysis[df_analysis$DayofMonth == 11 & df_analysis$TailNum ==  'N902UA', ]
head(rslt_df,2)

# At ORD airport flight ‘N348UA’ depart late. It suppose to depart at 19:00 (7.00 PM) but actually it is departed at 19:46 (7.46 PM) 46 minutes delayed. 
# Similarly we can observe same ‘N920UA’ delayed by 1 hr 5 minutes.


#--------------------------------------------------------------------------------------------------------------


# Q5 - Use the available variables to construct a model that predicts delays.

#Subsetting of data for modelling
df_modelling = select(df, DayofMonth,Year,DayOfWeek,UniqueCarrier,TailNum,Origin,Dest,Distance,CRSDepTime,DepTime,DepDelay,CRSArrTime,ArrTime,ArrDelay,
                      TaxiIn,TaxiOut,Cancelled,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay)


df_modelling = head(df_modelling,1000)

# Impute NA values using Mean imputation
df_modelling$DepTime[is.na(df_modelling$DepTime)] <- mean(df_modelling$DepTime, na.rm = T)
df_modelling$ArrTime[is.na(df_modelling$ArrTime)] <- mean(df_modelling$ArrTime, na.rm = T)
df_modelling$ArrDelay[is.na(df_modelling$ArrDelay)] <- mean(df_modelling$ArrDelay, na.rm = T)
df_modelling$DepDelay[is.na(df_modelling$DepDelay)] <- mean(df_modelling$DepDelay, na.rm = T)


df_modelling$ARR_DELAY = 0

#Masking the dependent variable with 0 and 1
df_modelling$ARR_DELAY[df_modelling$ArrDelay > 0] <- 1



################################################
#      Processing and Train Test split         # 
################################################


set.seed(1234)

ind = createDataPartition(df_modelling$ARR_DELAY,
                          times = 1,
                          p = 0.8,
                          list = F)
df_train = df_modelling[ind, ]
dim(df_train)
df_test = df_modelling[-ind, ]
dim(df_test)


################################################
#      Modelling using Logistic Regression     # 
################################################

logistic = glm(ARR_DELAY ~ DayofMonth+ Year+ DayOfWeek + Distance + CRSDepTime +CRSArrTime+
                 TaxiIn+ TaxiOut+ Cancelled + CarrierDelay + WeatherDelay + NASDelay + SecurityDelay,
               data = df_train,
               family = "binomial")

summary(logistic)



#Prediction using Logistic Regression

logistic_train_score = predict(logistic,
                               newdata = df_train,
                               type = "response")
logistic_test_score = predict(logistic,
                              newdata = df_test,
                              type = "response")




# creating classes according to score and cut
fun_cut_predict = function(score, cut) {
  # score: predicted scores
  # cut: threshold for classification
  
  classes = score
  classes[classes > cut] = 1
  classes[classes <= cut] = 0
  classes = as.factor(classes)
  
  return(classes)  
}


logistic_train_class = fun_cut_predict(logistic_train_score, 0.2)
#Confusion matrix for train and y
logistic_train_confm = confusionMatrix(logistic_train_class, as.factor(df_train$ARR_DELAY), 
                                       positive = "1",
                                       mode = "everything")
print(logistic_train_confm)


###########################################################
#    PLOTING AUC-ROC CURVE ON THE ALGORITH USED           # 
###########################################################


# plotting PR curve
gg_prcurve = function(df) {
  # df: df containing models scores by columns and the last column must be
  #     nammed "obs" and must contain real classes.
  
  # init
  df_gg = data.frame("v1" = numeric(), 
                     "v2" = numeric(), 
                     "v3" = numeric(), 
                     "model" = character(),
                     stringsAsFactors = F)
  
  # individual pr curves
  for (i in c(1:(ncol(df)-1))) {
    x1 = df[df$obs == 1, i]
    x2 = df[df$obs == 0, i]
    prc = pr.curve(x1, x2, curve = T)
    
    df_prc = as.data.frame(prc$curve, stringsAsFactors = F) %>% 
      mutate(model = colnames(df)[i])
    
    # combining pr curves
    df_gg = bind_rows(df_gg,
                      df_prc)
    
  }
  
  gg = df_gg %>% 
    ggplot() +
    aes(x = V1, y = V2, colour = model) +
    geom_line() +
    xlab("Recall") +
    ylab("Precision")
  
  return(gg)
}


score_train = data.frame("logistic complex" = logistic_train_score,
                         "obs" = as.numeric(df_train$ARR_DELAY) - 1)

roc_train = score_train %>%
  gather(key = "Method", value = "score", -obs) %>% 
  ggplot() +
  aes(d = obs,
      m = score,
      color = Method) +
  geom_roc(labels = F, pointsize = 0, size = 0.6) +
  xlab("Specificity") +
  ylab("Sensitivity") +
  ggtitle("ROC Curve", subtitle = "Train dataset")

prcurve_train = gg_prcurve(score_train) + ggtitle("PR Curve", subtitle = "Train dataset")
curves_train = ggarrange(roc_train, prcurve_train, 
                         common.legend = T,
                         legend = "bottom")

print(curves_train)






