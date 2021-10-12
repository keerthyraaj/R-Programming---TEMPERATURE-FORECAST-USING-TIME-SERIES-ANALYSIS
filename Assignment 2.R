##################################################
###                                             ##
### RStudio - Assignment 2                          ## 
##################################################
#                                               ##
##################################################
# Written by Keerthy Raaj Shanmugam
# ID: 8779954
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear all plots
if(!is.null(dev.list())) dev.off()

# Clear entire console
cat("\014") 

# Clean and clear theworkspace
rm(list=ls())

#Set work directory to an appropriate location
setwd("C:/Users/keert/Documents/Data")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(tseries)){install.packages("tseries")}
library("tseries")

if(!require(TTR)){install.packages("TTR")}
library("TTR")

if(!require(smooth)){install.packages("smooth")}
library("smooth")


#SECTION 1: Woodstock Temperature


#1.1 Read in the Woodstock data and transform it into an appropriate time series datatype.
woodstock_data_KS <- read.csv("Woodstock_21F.csv",header=TRUE,sep=",")
head(woodstock_data_KS)

Temp_TS_KS <- ts(woodstock_data_KS, frequency = 12, start=c(1988,1))
head(Temp_TS_KS)

#2.1 Summarize the temperature information (mean, etc.)
summary(Temp_TS_KS)

#2.2 Plot the time series data.
plot.ts(Temp_TS_KS, main="Average Temperature - Woodstock",  ylim = c(-14, 24)) 

#2.3 Decompose the times series data in to the constituent components. Comment on each (any trends you observe, etc.)
decomp_Temp_KS <- decompose(Temp_TS_KS, type="additive")  #Could be mult for multiplicative
decomp_Temp_KS
plot(decomp_Temp_KS)


#2.4 Determine if the time series is stationary.
adf.test(Temp_TS_KS) # p-value < 0.05  indicated series is stationary: note - can set the lags, k=n

#2.5 Deseasonalize the information and plot the result
Temp_TS_Seas_Adj_KS <- Temp_TS_KS - decomp_Temp_KS$seasonal

plot.ts(Temp_TS_Seas_Adj_KS, main="Deseasonalized - Avg Temperature - Woodstock",  ylim = c(-14, 24))


#SECTION 2: Ayr Temperature


#1.1 Read in the Ayr data and transform it into an appropriate time series datatype.
Ayr_data_KS <- read.csv("Ayr_21F.csv",header=TRUE,sep=",")
head(Ayr_data_KS)

Temp_TS_KS <- ts(Ayr_data_KS, frequency = 1, start=c(1968,1))
head(Temp_TS_KS)

#2.1 Summarize the information (mean, std dev, etc.)
summary(Temp_TS_KS)

#2.2 Plot the time series data
plot.ts(Temp_TS_KS, main="Average Temperature - Ayr",  ylim = c(10, 14)) 

#2.3 Smooth the temperature chart using a moving average. Try 3 different values for the moving average and choose the one you think best shows the trend (if any).
Temp_TS_SMA5_KS <-SMA(Temp_TS_KS,n=5)
plot.ts(Temp_TS_SMA5_KS)

Temp_TS_SMA10_KS <-SMA(Temp_TS_KS,n=10)
plot.ts(Temp_TS_SMA10_KS)

Temp_TS_SMA15_KS <-SMA(Temp_TS_KS,n=15)
plot.ts(Temp_TS_SMA15_KS)

#2.4 Determine if the time series is stationary.
adf.test(Temp_TS_KS)

#2.5 Create an autocorrelation chart (using acf) and comment on which lags are significant. Do previous values seem to influence current values?
acf(Temp_TS_KS)

#3.1 Create a simple moving average forecast of temperature in Ayr for five years beyond the data provided. Graph your results along with a 75% prediction interval.
move_avg_KS <- sma(Temp_TS_KS)
move_avg_KS

move_avg_KS <- forecast(move_avg_KS, h=5,level=0.75)
move_avg_KS
plot(move_avg_KS)

#3.2 Create an exponentially smoothed forecast of temperature in Ayr for five years beyond the data provided. Graph your results along with a 75% prediction interval.
ES_avg_KS <- es(Temp_TS_KS)
ES_avg_KS

ES_avg_KS <- forecast(ES_avg_KS, h=5,level=0.75)
ES_avg_KS
plot(ES_avg_KS)

