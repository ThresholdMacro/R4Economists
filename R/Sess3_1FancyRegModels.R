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
stargazer)

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
# y = alpha + beta1*educ + beta2*female + beta3*educ*female
model3 <- lm(wage~educ*female,data=wage1)
stargazer(model3,type='text')
#Just the interaction term use colon 
model4 <- lm(wage~educ:female+tenure+married,data=wage1)
stargazer(model4,type='text')

#Use log wage instead of wage
model5 <- lm(log(wage)~educ+tenure+female+married,data=wage1)
stargazer(model5,type='text')

#A set of dummies for education using factor
model6 <- lm(wage~factor(educ)+tenure+female+married,data=wage1)
stargazer(model6,type='text')

#Regression model with lots of control variables
regdata <- subset(wage1,select=c("wage","educ","exper","tenure","nonwhite","female","married","numdep"))
model7 <- lm(wage~.,data=regdata)
stargazer(model7,type='text')
regdata <- wage1[,1:8]
model7 <- lm(wage~.,data=regdata)
stargazer(model7,type='text')
#With an interaction
model8 <- lm(wage~educ*female+.,data=regdata)
stargazer(model8,type='text')

