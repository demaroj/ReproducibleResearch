## Loading and preprocessing the data

#library(ggplot2)
library(dplyr)
library(knitr)
#library(mice)

#set working dir
setwd("c:\\rprograms\\reproducibleresearch\\reproducibleresearch")

#download and unzip raw data from website
zipfile <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(zipfile, tf <- tempfile(fileext = ".bz2"))
#unzip(tf, exdir = td <- file.path(tempdir(), "myzip"))

# read csv data create dataframe
rawdata <- read.csv(tf)
tidydata <- rawdata[, c(1,2,7,23,24,25,26,27,28)]

tidydata$death_injury <- as.numeric(as.character(tidydata$FATALITIES))+as.numeric(as.character(tidydata$INJURIES))
#tidydata$economic <- as.numeric(as.character(tidydata$PROPDMG))*tidydata$PROPDMGEXP + as.numeric(as.character(tidydata$CROPDMG))*tidydata$CROPDMGEXP

# fun <- function(x){dplyr::case_when(tidydata$PROPDMGEXP == "M" ~ 1e6,
#                                     tidydata$PROPDMGEXP == "B" ~ 1e9,
#                                     tidydata$PROPDMGEXP == "K" ~ 1e3,
#                                     TRUE ~ 1)}
# expRows <- length(tidydata$PROPDMGEXP)
# tidydata$PROPDMGEXP_convert <- (sapply(1:expRows,FUN = fun))*1

# for(i in 1:length(tidydata$PROPDMGEXP)) {
#   if(tidydata[i,]$PROPDMGEXP=='M') {tidydata[i,]$PROPDMG_convert <- tidydata[i,]$PROPDMG*1e6}
#   if(tidydata[i,]$PROPDMGEXP=='K') {tidydata[i,]$PROPDMG_convert <- tidydata[i,]$PROPDMG*1e3}
# }

exponent_Finance <- tidydata$PROPDMGEXP 
exp_value <- sapply(exponent_Finance, function(x)if (x=='K') {1000} else if (x=='M') {1000000} else if (x=='B') {1000000000} else {0})


# fun <- function(x){
#   print(x)
#   if(x='M'){temp<-as.numeric(as.character(tidydata$PROPDMG))*1e6
# }}
# expRows <- length(tidydata$PROPDMGEXP)
# tidydata$PROPDMGEXP_convert <- sapply(1:expRows, FUN=fun(as.character(tidydata$PROPDMGEXP)))
# 
# # tidydata$PROPDMGEXP_convert <- sapply(tidydata$PROPDMGEXP, function(x)(as.numeric(temp<-gsub("B", "", x))*1000000000))
# # tidydata$PROPDMGEXP_convert <- sapply(tidydata$PROPDMGEXP, function(x)(as.numeric(temp<-gsub("M", "", x))*1000000))
# # tidydata$PROPDMGEXP_convert <- sapply(tidydata$PROPDMGEXP, function(x)(as.numeric(temp<-gsub("K", "", x))*1000))
