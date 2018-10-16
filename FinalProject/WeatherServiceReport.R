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

###Create tidydata as a subset of the rawdata - use visual inspection to help
### columns include are event, fatalities, injuries, property costs with exponent and crop costs with exponent column
tidydata <- rawdata[, c(8,23,24,25,26,27,28)]

###add column to sum fatalities and injury number 
tidydata$populationEffect <- as.numeric(as.character(tidydata$FATALITIES))+as.numeric(as.character(tidydata$INJURIES))

###create a vector to hold and allow for conversion of exponent coding
exponent_Prop <- tidydata$PROPDMGEXP 
exp_prop <- sapply(exponent_Prop, function(x)  if (x=='K') {1000} else if (x=='M') {1000000} else if (x=='H') {100} else if (x=='B') {1000000000} else {0})
###create a vector to hold and allow for conversion of exp coding
exponent_Crop <- tidydata$CROPDMGEXP 
exp_crop <- sapply(exponent_Crop, function(x) if (x=='K') {1000} else if (x=='M') {1000000} else if (x=='H') {100} else if (x=='B') {1000000000} else {0})
###bind new columns to tidydata set containing number value for alpha label
tidydata <- cbind(tidydata, exp_prop, exp_crop)

###add column that calculates property and crop damage in $                     
tidydata$economicEffect <- (as.numeric(as.character(tidydata$PROPDMG))*as.numeric(as.character(tidydata$exp_prop)) + as.numeric(as.character(tidydata$CROPDMG))*as.numeric(as.character(tidydata$exp_crop)))



summarypopulationData <- aggregate(cbind(tidydata$EVTYPE, tidydata$populationEffect), by=list(tidydata$EVTYPE), FUN=sum, na.rm=TRUE, na.action=NULL)
summaryeconomicData <- aggregate(cbind(tidydata$EVTYPE, tidydata$economicEffect), by=list(tidydata$EVTYPE), FUN=sum, na.rm=TRUE, na.action=NULL)
colnames(summarypopulationData) <- c("Event","V1", "populationEffect")
colnames(summaryeconomicData) <- c("Event", "V1", "economicEffect")

summarypopulationData <- summarypopulationData[order(-summarypopulationData$populationEffect), ]

###plot graph
ggplot(data=head(summarypopulationData ,10), aes(x=Event, y=populationEffect)) + 
     stat_summary(fun.y=sum, geom="bar", color = "black", size=1) +
     theme( axis.ticks.x=element_blank())+
     coord_flip()

summaryeconomicData <- summaryeconomicData[order(-summaryeconomicData$economicEffect), ]


ggplot(data=head(summaryeconomicData,10), aes(x=Event, y=economicEffect)) + 
     stat_summary(fun.y=sum, geom="bar", color = "black", size=1) +
     theme(axis.ticks.x=element_blank())+
     coord_flip()