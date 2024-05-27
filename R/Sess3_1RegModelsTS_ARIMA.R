## ---------------------------
##
## Script name: FancyRegModels.R
## Purpose of script:
## Author: Meyrick Chapman
## Date Created: Mon May 06 2024
## Copyright (c) Hedge Analytics Ltd, 2024
## Email: meyrick@hedge-analytics.com
##
## ---------------------------
## Notes:
##   
## ---------------------------

## set working directory

## project directory = default 

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
ggthemes,
sysfonts,
showtext,
foreign,
stargazer,
sandwich,#Sandwich is for heteroskedasticity-robust and cluster-robust errors 
lmtest,#lmtest is for Breusch-Pagan
AER)#AER is for linearHypothesis

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
# source("~/Library/Mobile Documents/com~apple~CloudDocs/Meyrick/R/Libs/HAgraphics/R/ggstdplots.R")

## ---------------------------

#Read in Wooldridge data
wage1 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wage1.dta")

#Basic model using lm() function [lm = linear model]
model1 <- lm(wage~educ+tenure+female+married,data=wage1)
summary(model1)
stargazer(model1,type='text')

#Include a squared term
model2 <- lm(wage~educ+I(educ^2)+tenure+female+married,data=wage1)
stargazer(model2,type='text')

#Variables and their interactions
#interaction terms enable you to examine whether the relationship 
#between the target and the independent variable changes 
#ßdepending on the value of another independent variable.
# y = alpha + beta1*educ + beta2*female + beta3*educ*female
model3 <- lm(wage~educ*female,data=wage1)
stargazer(model3,type='text')
#Just the interaction term use colon 
model4 <- lm(wage~educ:female+tenure+married,data=wage1)
stargazer(model4,type='text')

#Use log wage instead of wage - seems a good idea
model5 <- lm(log(wage)~educ+tenure+female+married,data=wage1)
stargazer(model5,type='text')

#A set of dummies for education using factor
model6 <- lm(wage~factor(educ)+tenure+female+married,data=wage1)
stargazer(model6,type='text')
# worth looking at what is going on with factor() 
sort(unique(wage1$educ))

#Regression model with lots of control variables
regdata <- subset(wage1,select=c("wage","educ","exper","tenure","nonwhite","female","married","numdep"))
model7 <- lm(wage~.,data=regdata)
stargazer(model7,type='text')
#With an interaction
model8 <- lm(wage~educ*female+.,data=regdata)
stargazer(model8,type='text')


# -----------------------------------
# test for heteroskedasticitic errors

model3 <- lm(wage~educ+tenure+female+married,data=wage1)
stargazer::stargazer(model3, type = 'text')
# Heteroskedasticity-consistent standard errors that differ from classical standard errors 
# may indicate model misspecification. Substituting heteroskedasticity-consistent standard errors 
# does not resolve this misspecification, which may lead to bias in the coefficients. 
# In most situations, the problem should be found and fixed. 
# Other types of standard error adjustments, such as clustered standard errors or HAC standard errors, 
# may be considered as extensions to HC standard errors.
#Robust standard errors

# vcovHC = "Heteroscedasticity-Consistent Covariance Matrix Estimation"
RobSE <- lmtest::coeftest(model3,vcovHC)
stargazer::stargazer(model3,coeftest(model3,vcovHC),type='text')
#Clustered standard errors
# vcovCL = "Clustered Covariance Matrix Estimation"
ClustSE <- lmtest::coeftest(model3,vcovCL,cluster=wage1$smsa)
stargazer::stargazer(model3,RobSE,ClustSE,type='text')

# WARNING: robust standard error functions may contain default settings which differ from package to package
# and between computer languages. Results may therefore be dependent on assumptions not visible to the user
# See: https://stackoverflow.com/questions/65435476/why-panelols-in-python-has-different-result-than-plm-in-r


#F test of significance
#Test if the coefficient on educ is 0 
#(note that the p-value of this test will be exactly the same 
#as the p-value of the t-test for that coefficient reported in
#the regression table
car::linearHypothesis(model3,"educ=0",type=c("F"))
#Joint test that the coefficient on educ AND the coefficient on female
#are both 0
car::linearHypothesis(model3,c("educ=0","female=0"),type=c("F"))
#Joint test of every variable in the model. The F-statistic here
#is the same F-statistic reported for the whole model at the
#bottom of the regression table.
car::linearHypothesis(model3,c("educ=0","female=0","tenure=0","married=0"),type=c("F"))


# --------------------------------
# Time series

library(tseries)
library(dynlm)
library(forecast)

#Create random data
gdpgrowth <- rnorm(100)

#Declare it as time series with ts()
gdpgrowth <- ts(gdpgrowth,start=c(1990,1),frequency=12)

#Plot our time series
plot(gdpgrowth)

#Bring in some China data from AER package
data("ChinaIncome")

#Plot China data
plot(ChinaIncome)

#Pull out one of the time series
industry <- ChinaIncome[,4]
plot(industry)

#Autocorrelation plot
acf(industry)
#Partial autocorrelation function
pacf(industry)

#Dickey-Fuller unit root test for autoregression 
tseries::adf.test(industry)

#Run a regression using a lag value
tsreg <- dynlm(industry~L(industry))
stargazer::stargazer(tsreg,type='text')
# and more than one lag
tsreg2 <- dynlm(industry~L(industry,1:3))
stargazer::stargazer(tsreg2,type='text')

# decompose a timeseries into trend, seasonal, error terms
USretailadj <- alfred::get_fred_series(series_id = 'RSXFS', series_name = 'Retail Adj')
USretailadj$date <- as.Date(USretailadj$date)
USretailnonadj <- alfred::get_fred_series(series_id = 'RSXFSN', series_name = 'Retail Nonadj')
USretailnonadj$date <- as.Date(USretailnonadj$date)
tsUSretailnonadj <- ts(USretailnonadj$`Retail Nonadj`, frequency = 12, start = c(1992,1))
tsUSretailadj <- ts(USretailadj$`Retail Adj`, frequency = 12, start = c(1992,1))


USRetailtimeseriescomponents <- decompose(tsUSretailnonadj)
plot(USRetailtimeseriescomponents)

plot(stl(tsUSretailnonadj,s.window="period"))


# ---------------------------------------
# ARIMA models

# An autoregressive integrated moving average, or ARIMA, 
# is a statistical analysis model that uses time series data
# to either better understand the data set or to predict future trends. 
# parameters
# p: the number of lag observations in the model, also known as the lag order.
# d: the number of times the raw observations are differenced; also known as the degree of differencing.
# q: the size of the moving average window, also known as the order of the moving average.


plot(
  tsUSretailnonadj,
  type = 'l',
  main = "US Retail Sales - non-adjusted",
  ylab=("Retail index"),
xlab=("Year"))

lines(
  tsUSretailadj,
  col='red')

plot(
  tsUSretailadj,
  type = 'l',
  main = "US Retail Sales - adjusted",
  ylab=("Retail index"),
  xlab=("Year"))

# find the appropriate seasonal difference

tsUSretailnonadj %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

# significant spike at ACF 2 and 12 
# as monthly data this suggests both high seasonality and possible quarterly influence
arima(tsUSretailnonadj,order=c(2,1,1),seasonal = c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()
arimamodel <- arima(tsUSretailnonadj,order=c(2,1,1),seasonal = c(0,1,2))
summary(arimamodel)
stargazer(arimamodel,type= 'text')
checkresiduals(arimamodel)

arimamodel %>% forecast(h=12) %>% autoplot()



tsUSretailadj %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

# significant spike at ACF 2 and 12 
arima(tsUSretailadj,order=c(2,1,1),seasonal = c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()
arimamodel <- arima(tsUSretailadj,order=c(0,1,1),seasonal = c(0,1,1))
summary(arimamodel)
stargazer(arimamodel,type= 'text')
checkresiduals(diff(tsUSretailadj))
arimamodel %>% forecast(h=12) %>% autoplot()

tseries::adf.test(diff(tsUSretailadj,lag = 12))
plot(diff(tsUSretailadj))
