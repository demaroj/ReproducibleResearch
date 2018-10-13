#Analysis of NOAA Data to Visualize Differences in Population Health and Economics by Weather Event Types 
 
###Synopsis: Raw data collected by NOAA provides event based data for every storm event in the United States for over 50 years.  


###Loading and preprocessing the data
library(ggplot2)
library(dplyr)
library(knitr)

###set working dir
setwd("C:\\RPrograms\\ReproducibleResearch\\ReproducibleResearch\\FinalProject\\ReproducibleResearch\\FinalProject")

###download and unzip raw data from website
datafile <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(datafile, tf <- tempfile(fileext = ".bz2"))

###read csv data and create rawdata dataframe
rawdata <- read.csv(tf)
###Create tidydata as a subset of the rawdata
tidydata <- rawdata[, c(1,2,7,8, 23,24,25,26,27,28)]

###add column to sum fatalities and injury number 
tidydata$death_injury <- as.numeric(as.character(tidydata$FATALITIES))+as.numeric(as.character(tidydata$INJURIES))

###create a vector to hold and allow for conversion of exp coding
exponent_Finance <- tidydata$PROPDMGEXP 
exp_value <- sapply(exponent_Finance, function(x)  if (x=='K') {1000} else if (x=='M') {1000000} else if (x=='H') {100} else if (x=='B') {1000000000} else {0})
###create a vector to hold and allow for conversion of exp coding
exponent_Crop <- tidydata$CROPDMGEXP 
exp_crop <- sapply(exponent_Crop, function(x) if (x=='K') {1000} else if (x=='M') {1000000} else if (x=='H') {100} else if (x=='B') {1000000000} else {0})
###bind new columns to tidydata set containing number value for alpha label
tidydata <- cbind(tidydata,exp_value, exp_crop)

###add column that calculates property and crop damage in $                     
tidydata$economic <- (as.numeric(as.character(tidydata$PROPDMG))*as.numeric(as.character(tidydata$exp_value)) + as.numeric(as.character(tidydata$CROPDMG))*as.numeric(as.character(tidydata$exp_crop)))

summaryEconData <- aggregate(cbind(tidydata$EVTYPE, tidydata$death_injury, tidydata$economic), by=list(tidydata$EVTYPE), FUN=sum, na.rm=TRUE, na.action=NULL)

#summarydata <- tidydata[order(-tidydata$economic), ]

colnames(summaryEconData) <- c("Event", "Death_Injury", "Economic")
summaryEconData <- summaryEconData[order(-summaryEconData$Death_Injury), ]
head(summaryEconData)

###plot graph
ggplot(data=head(summaryEconData,10), aes(x=Event, y=Death_Injury)) + 
     stat_summary(fun.y=sum, geom="bar", color = "black", size=1) +
     theme( axis.ticks.x=element_blank())

summaryEconData <- summaryEconData[order(-summaryEconData$Economic), ]
head(summaryEconData)

ggplot(data=head(summaryEconData,10), aes(x=Event, y=Economic)) + 
     stat_summary(fun.y=sum, geom="bar", color = "black", size=1) +
     theme(axis.ticks.x=element_blank())