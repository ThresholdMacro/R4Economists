## ---------------------------
##
## Script name: 
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

model3 <- lm(wage~educ+tenure+female+married,data=wage1)
stargazer::stargazer(model3, type = 'text')
# Heteroskedasticity-consistent standard errors that differ from classical standard errors 
# may indicate model misspecification. Substituting heteroskedasticity-consistent standard errors 
# does not resolve this misspecification, which may lead to bias in the coefficients. 
# In most situations, the problem should be found and fixed. 
# Other types of standard error adjustments, such as clustered standard errors or HAC standard errors, 
# may be considered as extensions to HC standard errors.
#Robust standard errors
RobSE <- lmtest::coeftest(model3,vcovHC)
stargazer::stargazer(model3,coeftest(model3,vcovHC),type='text')
#Clustered standard errors
ClustSE <- lmtest::coeftest(model3,vcovCL,cluster=wage1$smsa)
stargazer::stargazer(model3,RobSE,ClustSE,type='text')

#Breusch-Pagan test for Heteroskedasticity - test strongly rejects 
lmtest::bptest(model3)

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

