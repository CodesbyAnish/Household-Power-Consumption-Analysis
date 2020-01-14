#install necessary packages
options(warn=-1)
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
library(data.table)
library(ggplot2)
library(dplyr)
library(xtable)
library(lubridate)
library(reshape2)
install.packages("TTR")
library(TTR)
install.packages("forecast")
library(forecast)
options(warn=0)
setwd("C:/Users/anish/Downloads/Data Management and Big Data/Final Project")

#Input file
hpcFile <- "C:/Users/anish/Downloads/Data Management and Big Data/Final Project/household_power_consumption.txt"
HHPC <- read.table(hpcFile, header= TRUE,sep=";",as.is=TRUE)
head(HHPC)

#data cleaning
HHPC$Global_active_power <- as.numeric(HHPC$Global_active_power)
HHPC$Global_reactive_power <- as.numeric(HHPC$Global_reactive_power)
HHPC$Voltage <- as.numeric(HHPC$Voltage)
HHPC$Global_intensity <- as.numeric(HHPC$Global_intensity)
HHPC$Sub_metering_1 <- as.numeric(HHPC$Sub_metering_1)
HHPC$Sub_metering_2 <- as.numeric(HHPC$Sub_metering_2)

#initial dataframe check
HHPC <-cbind(HHPC, as.Date(HHPC$Date, "%d/%m/%Y"), stringsAsFactors=FALSE)
colnames(HHPC)[10] <-"DateFormat"
HHPC <- HHPC[,c(ncol(HHPC), 1:(ncol(HHPC)-1))]
head(HHPC)
str(HHPC)

#cleaning 2
HHPC <-cbind(HHPC, month(HHPC$DateFormat, label = TRUE, abbr = TRUE), stringsAsFactors=FALSE) #month extraction
colnames(HHPC)[11] <-"Month"
HHPC <- HHPC[,c(ncol(HHPC), 1:(ncol(HHPC)-1))]

HHPC <-cbind(HHPC, year(HHPC$DateFormat), stringsAsFactors=FALSE) #year extraction
colnames(HHPC)[12] <-"Year"
HHPC <- HHPC[,c(ncol(HHPC), 1:(ncol(HHPC)-1))]

HHPCsml <- HHPC
HHPCsml$DateFormat <- NULL #eliminate unnecessary cols
HHPCsml$Date <- NULL
HHPCsml$Time <- NULL

HHPCTrain <- HHPCsml #create training set
head(HHPCTrain,5)

#cleaning 3 for time series forecasting

HHPCsml <- group_by(HHPCsml, Year, Month) #grouping month & year
HHPCsml <- summarise(HHPCsml, MeanGAP = mean(Global_active_power, na.rm = TRUE),
                     MeanGRP = mean(Global_reactive_power, na.rm = TRUE),
                     MeanVolt = mean(Voltage, na.rm = TRUE),
                     MeanGI = mean(Global_intensity, na.rm = TRUE),
                     MeanSubm1 = mean(Sub_metering_1, na.rm = TRUE),
                     MeanSubm2 = mean(Sub_metering_2, na.rm = TRUE),
                     MeanSubm3 = mean(Sub_metering_3, na.rm = TRUE))
HHPCsml <- arrange(HHPCsml, Year, Month) #reducing data using mean


GAP_vector <- vector( mode ="numeric") #mean global active power calculation
GAP_vector <- HHPCsml$MeanGAP
GAP_ts <- ts(GAP_vector, frequency = 12, start = c(2006,12), end = c(2010,11))
plot.ts(GAP_ts)

GAP_ts_SMA3 <- SMA(GAP_ts, n=3) #smoothing curve using moving average parameter
plot.ts(GAP_ts_SMA3)

#time series analysis and forecasting

GAP_ts_components <- decompose(GAP_ts)
plot(GAP_ts_components)

GAPforecasts <- HoltWinters(GAP_ts, beta=FALSE, gamma=FALSE)
GAPforecasts
plot(GAPforecasts)

GAPforecasts$SSE #sum of squared errors


#confidence interval forecast
GAPforecasts2 <- forecast:::forecast.HoltWinters(GAPforecasts, h=12)
GAPforecasts2
plot(GAPforecasts2)

