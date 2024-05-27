## ---------------------------
##
## Script name: Sess2_1EDA.R
## Purpose of script: Exploratory data analysis in R - basic demonstration
## Author: Meyrick Chapman
## Date Created: Tue May 07 2024
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
rdbnomics,
DT,
xts)


## ---------------------------
source("R/ggstdplots.R")

## ---------------------------

startdate <- as.Date("1999-01-01")

# ----------------------------
# We are going to use HICP data from Eurostat, as provided by db.nomics.world
# why not go to Eurostat? We could, but as we said in Session 1, db.nomics.world provides a huge set of data in standard format from many providers
# so, knowing how to get data from db.nomics.world is very useful. 
# There are other comprehensive sites, such as FRED (Federal Reserve Economic Database) hosted by St Louis Fed
# FRED uses its own API functions in the alfredr & fredr packages
# You need an API key for FRED. This is free and allows for recursive/multiple calls to the database. 
# Have you got your API yet?
# 
# db.nomics.world
# Get data from db.nomics.world. Note the parameters we provide. 
# Here is the URL for the series on the website: https://db.nomics.world/Eurostat/prc_hicp_fp?q=M.RCH_A.CP00..
# ----------------------------
EZ_HICP <- rdbnomics::rdb(provider_code = "Eurostat",dataset_code = "prc_hicp_manr", ids = "M.RCH_A.CP00..")

# ----------------------------
# Let's look at what we got
# ----------------------------
View(EZ_HICP)

# ----------------------------
# use dplyr & lubridate to select the data we need from the longer dataset
# ----------------------------

EZ_HICPsht <-
  dplyr::select(EZ_HICP,period,`Geopolitical entity (reporting)`, value)
  
EZ_HICPsht <-
dplyr::filter(EZ_HICPsht, period>=as.Date(startdate))

# ----------------------------
# use dplyr & lubridate to get the data, but in a shorter syntax using the pipe function ("|>")
# ----------------------------

EZ_HICPsht <-
  EZ_HICP |>
  dplyr::select(period,`Geopolitical entity (reporting)`, value)|>
  dplyr::filter(period>=as.Date(startdate))

colnames(EZ_HICPsht) <- c( "Date", "Country","value")
EZ_HICPsht$Date <- lubridate::ceiling_date(EZ_HICPsht$Date, unit = 'months')-1

# ----------------------------
# built-in tidyr function to arrange data into a 'wide' format, which makes it easier to read for humans
# ----------------------------
EZ_HICPwide <-
  EZ_HICPsht |>
  tidyr::pivot_wider(id_cols =Date, names_from = 'Country', values_from = 'value')

EZ_HICPwide <-
  EZ_HICPwide |>
  arrange(Date)

# ============================
# 1. Look at the data, explore the structure of the data
# ============================
# what are the dimensions of the data we have?
# ----------------------------

dim(EZ_HICPwide)

# ----------------------------
# peek at the data
# ----------------------------

head(EZ_HICPwide)
tail(EZ_HICPwide)

summary(EZ_HICPwide)

# also look at the data in a nicer format using DT
DT::datatable(data= EZ_HICPwide)
# some issues with display, so let's get rid of pesky columns with very long names

DT::datatable(data= EZ_HICPwide[,-c(11,15,18)])

# let's get rid of these columns
# that's the easy option. It may be better to rename them, therefore keeping the data

EZ_HICPwide <-
  EZ_HICPwide[,-c(11,15,18)]

DT::datatable(data= EZ_HICPwide)

# ============================
# 2. Visualise and analysing the data
# ============================

# ----------------------------
# visualise time series the basic way
# we'll only look at a subset of the data - Germany, France, Italy & Spain
# ----------------------------

plot(EZ_HICPwide$Date, EZ_HICPwide$Germany, type = 'l', main = 'Headline HICP: major EZ countries')
lines(EZ_HICPwide$Date, EZ_HICPwide$France, col = "red")
lines(EZ_HICPwide$Date, EZ_HICPwide$Italy, col = "green")
lines(EZ_HICPwide$Date, EZ_HICPwide$Spain, col = "blue")
legend(
  'topleft',
  c("Germany", "France", "Italy", "Spain"),
  lty = c(1, 1),
  col = c("black", "red", "green", "blue"),
  text.col = c("black", "red", "green", "blue")
)

# there are ways to prettify this base graphic
# choose the subset of column names as a vector to use in future functions
# ----------------------------

srs_chosen <- c('Germany','France','Italy','Spain')

# how to use the vector of column names
# ----------------------------
View(EZ_HICPwide[,srs_chosen])

# ----------------------------
# convert to time series object - using our vector of column names
# time series objects are a useful way to view and manipulate data 
# the following uses xts (extensible time series) format, but there are others 'zoo', ts. 
xts_EZ_HICP <- xts::xts(EZ_HICPwide[,srs_chosen], order.by=as.Date(EZ_HICPwide$Date))


plot.xts(xts_EZ_HICP, auto.legend = TRUE, main = 'Headline HICP: major EZ countries')

xts::addLegend(legend.loc="topleft", legend.names=names(xts_EZ_HICP), 
               lty = 1, col=1:ncol(xts_EZ_HICP),text.col=1:ncol(xts_EZ_HICP), bg="white", bty=1)

# ----------------------------
# another time series set of time series functions are in the 'zoo' package

zoo_EZ_HICP <- zoo::zoo(x=EZ_HICPwide[,srs_chosen],order.by=as.Date(EZ_HICPwide$Date))
plot(zoo_EZ_HICP)

# ----------------------------
# not the same output, but zoo is very useful for applying functions to time series
# such as 'roll' series of functions which create a result from a rolling window in a time series

zoo::rollmean(zoo_EZ_HICP$Germany, k=6)

# ----------------------------
# visualise a subset of the data using more modern, flexible approach (ggplot2)
# ----------------------------
# long format needed for ggplot2
selectHICP <-
  EZ_HICPsht |>
  filter(Country %in% srs_chosen)

p_HICPyoy <- ggplot(selectHICP, aes(x = Date, y = value, colour = Country)) + 
  geom_line() + 
  labs(title = "HICP annual % change for selected countries",
       y = "Percent change 12 months")

p_HICPyoy
p_HICPyoy + darktheme + theme(legend.position = 'bottom') + scale_color_HA_qualitative()


# ============================
# 3. Identification of missing values and missing value treatment
# ============================

# missing values?
head(EZ_HICPwide)
tail(EZ_HICPwide)
summary(EZ_HICPwide)

# choose one column to show lots of NAs
EZ_HICPwide$`United Kingdom`

# we could use a 'for' loop to find how many NAs are in each column
# for loops are very useful, but can be slow and hard to understand/debug
# this one is simple so it may be the right way to go

#create a dataframe called numNAs
numNAs <- data.frame("Country"=NA,"NAs"=NA)
for(i in 1:ncol(EZ_HICPwide)){
  
  numNAs[i,] <- c(names(EZ_HICPwide[,i]), sum(is.na(EZ_HICPwide[,i])))
  
}

# the "apply" family of functions ("apply","lapply","sapply","vapply") vectorise transformations, which otherwise require a loop.
# vectorisation is generally preferred because it is much faster and (once familiar), easier to follow
# the following code says apply the function sum(is.na) to all columns dataframe "EZ_HICPwide"
# which is the same as asking how many NAs are in each column
numNAs2 <- data.frame(sapply(EZ_HICPwide, function(x) sum(is.na(x))))

# the sapply version is a bit different in format, but the same data and easier to specify (once you are familiar with it)

# there are clearly a lot of NAs
# what to do? exclude NAs? 
df <- tail(na.omit(EZ_HICPwide))

# oops! that excludes too much - blame Brexit! 
# first, let's only exclude missing values from single columns

# exclude columns with NAs. This means any column with NAs will be excluded
# look at the difference to the earlier dataframe 
head(EZ_HICPwide)
EZ_HICPwide[ , sapply(EZ_HICPwide, function(x) !any(is.na(x)))]
# this shows how is.na can seriously affect your data. That may be what you want, or it may not.
EZ_HICPwideNoNA <- EZ_HICPwide[ , sapply(EZ_HICPwide, function(x) !any(is.na(x)))]


#what happens if we want to keep NAs? 
# no problem, it will plot, of course, but there will be omissions in the series

plot(EZ_HICPwide$Date, EZ_HICPwide$Germany, type = 'l', main = 'Headline HICP: major EZ countries')
lines(EZ_HICPwide$Date, EZ_HICPwide$`United Kingdom`, col = "red")

legend(
  'topleft',
  c("Germany", "United Kingdom"),
  lty = c(1, 1),
  col = c("black", "red"),
  text.col = c("black", "red")
)

# or using xts

srs_chosen <- c('Germany','United Kingdom')

xts_EZ_HICP <- xts::xts(EZ_HICPwide[,srs_chosen], order.by=as.Date(EZ_HICPwide$Date))

plot.xts(xts_EZ_HICP, auto.legend = TRUE, main = 'Headline HICP: major EZ countries')

xts::addLegend(legend.loc="topleft", legend.names=names(xts_EZ_HICP), 
               lty = 1, col=1:ncol(xts_EZ_HICP),text.col=1:ncol(xts_EZ_HICP), bg="white", bty=1)


# and ggplot2

selectHICP <-
  EZ_HICPsht |>
  filter(Country %in% srs_chosen)

p_HICPyoy <- ggplot(selectHICP, aes(x = Date, y = value, colour = Country)) + 
  geom_line() + 
  labs(title = "HICP annual % change for selected countries",
       y = "Percent change 12 months")

p_HICPyoy

# ============================
# 4. Exploring numeric data attributes
# ============================

# ----------------------------
# analyse a subset of the data
# ----------------------------

# ----------------------------
# 4.1 Measure the central tendency
# ----------------------------
# mean
mean(EZ_HICPwideNoNA$Germany)

median(EZ_HICPwideNoNA$Germany)

mode(EZ_HICPwideNoNA$Germany)
# oops! mode() tells what kind of structure the object is!
# there is no inbuilt mode function in R
# why? I don't know, but it is easy to create one. 
# create a function Modes() which will work on more than one mode (i.e. bi-modal) data. 

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

Modes(EZ_HICPwideNoNA$Germany)

# ----------------------------
# 4.2 The shape of the distribution
# ----------------------------
# interquartile range (50% of data lies within this range around the mean)
IQR(EZ_HICPwideNoNA$Germany,na.rm=TRUE)

quantile(EZ_HICPwideNoNA$Germany, seq(from = 0, to = 1, by = 0.01))
plot(quantile(EZ_HICPwideNoNA$Germany, seq(from = 0, to = 1, by = 0.01)), main = "Distribution of HICP by quantile",
     xlab="Quantile", ylab="Germany monthly HICP since 1999")

var(na.omit(EZ_HICPwideNoNA$Germany))
sd(na.omit(EZ_HICPwideNoNA$Germany))


# digression - 




library(moments)
skewness(EZ_HICPwideNoNA$Germany)
# highly positively skewed

# histograms are useful
# simple
hist(EZ_HICPwideNoNA$Germany)
# simple with more detail
hist(EZ_HICPwideNoNA$Germany, breaks = seq(
  from = min(EZ_HICPwideNoNA$Germany),
  to = max(EZ_HICPwideNoNA$Germany),
  by = 0.1
))

srs_chosen <- c('Germany','France','Italy','Spain')
selectHICP <-
  EZ_HICPsht |>
  filter(Country %in% srs_chosen)


# less simple, but multiple histograms on same page
# notice we're using an earlier long dataset (selectHICP) for ggplot
p_HICPscatter <- ggplot(data = selectHICP, aes(x = value)) + geom_histogram(binwidth = 0.1)
p_HICPscatter + facet_wrap(~Country)
# the charts are in alphabetical order, not in the order we specified, let's fix that by ordering the factors of the variable 'Country'
p_HICPscatter + facet_wrap(~factor(Country, levels=srs_chosen))

# ----------------------------
# 4.3 Explore the spread using Visualization plots 
# 4.4 Identify outliers and unusual observations
# ----------------------------
# a boxplot to show 50% of occurances & mean
# simple

boxplot(value ~ Country, data = selectHICP)
title("HICP annual % change for selected countries")
# Again, the order of countries has changed to alphabetical. 
# We don't want that so need to reorder back to our original order by reordering the factors
boxplot(value ~ fct_reorder(Country, value), data = selectHICP, xlab = 'Country')
title("HICP annual % change for selected countries")

# less simple, but flexible
ggplot(data = selectHICP, mapping = aes(x = Country, y = selectHICP$value)) +
  geom_boxplot()+ 
  labs(title = "HICP annual % change for selected countries",
       y = "Percent change 12 months")

# Yet again, the order of the countries is alphabetical. Let's reorder back to original order using factors
ggplot(data = selectHICP, mapping = aes(x = fct_reorder(Country,value), y = selectHICP$value)) +
  geom_boxplot()+ 
  labs(title = "HICP annual % change for selected countries",
       y = "Percent change 12 months",
       x = 'Country')

# ============================
# 5. Exploring relationship between features (Bivariate & Multivariate analysis)
# ============================
# 5.1 Correlation
# ----------------------------

ccf(EZ_HICPwideNoNA$Germany, EZ_HICPwideNoNA$Lithuania)

# that may not be very informative - too similar
# let's create a dataframe of differences over 12 moonths

mthsdiff <- 12

diffEZ_HICPwideNoNA <-
  data.frame(
  EZ_HICPwideNoNA[,-1] |>
  sapply(diff,mthsdiff))

# a date series 
diffDates <- EZ_HICPwideNoNA$Date[mthsdiff+1:(nrow(EZ_HICPwideNoNA)-mthsdiff)]
diffEZ_HICPwideNoNA <- cbind("Date"=diffDates,diffEZ_HICPwideNoNA)

ccf(diffEZ_HICPwideNoNA$Germany, diffEZ_HICPwideNoNA$Lithuania)
# we can't really tell if German inflation highest correlation is above 0 on the x-axis
# let's zoom in
ccf(diffEZ_HICPwideNoNA$Germany, diffEZ_HICPwideNoNA$Lithuania, lag.max = 5)

# we can also retain the output from the ccf function 
ccfdata <- ccf(diffEZ_HICPwideNoNA$Germany, diffEZ_HICPwideNoNA$Lithuania)
ccfdata <- cbind(ccfdata$lag, ccfdata$acf)

View(ccfdata)

# ----------------------------
# 5.2 Visualization of relationship using scatter plot
# ----------------------------
# base graphics



plot(
  EZ_HICPwideNoNA$Germany,
  EZ_HICPwideNoNA$Lithuania,
  main = 'Scatter plot of HICP, Germany vs. Lithuania',
  xlab = 'Germany',
  ylab = 'Lithuania'
)

# create a linear regression of the 2 variables
fit1 <- lm(Lithuania ~ Germany, data = EZ_HICPwide)
summary(fit1)
# a nicer way to display 
stargazer::stargazer(fit1,type = 'text')

# add to the plot
abline(fit1, col = "blue")

model_residuals = fit1$residuals
hist(model_residuals)

# Plot the residuals
qqnorm(model_residuals)
# Plot the Q-Q line
# plotting them will give a straight line if they are normally distributed
qqline(model_residuals)
# obviously, there are major outliers in the relationship between Lithuanian and German HICP
# the relationship is not normally distributed, which is not surprising, given different economy structures

# ggplot2 graphics
p_scatter <-
  ggplot(data = EZ_HICPwideNoNA, mapping = aes(x = Germany, y = Lithuania)) +
  geom_point() +
  labs(title = 'Scatter plot of HICP, Germany vs. Lithuania', x = 'Germany', y = 'Lithuania') +
  geom_smooth(method = lm, se = TRUE)

p_scatter

p_scatter + labs(subtitle = paste("Adj R2 = ",signif(summary(fit1)$adj.r.squared, 3),
"Intercept =",signif(fit1$coef[[1]],3 ),
" Slope =",signif(fit1$coef[[2]], 3),
" P =",signif(summary(fit1)$coef[2,4], 3)))


# what about differences? 
plot(
  diffEZ_HICPwideNoNA$Germany,
  diffEZ_HICPwideNoNA$Lithuania,
  main = '12 mth differences of annual HICP, Lithuania vs. Germany',
  xlab = 'Germany',
  ylab = 'Lithuania'
)

# create a linear regression of the 2 variables
fit2 <- lm(Lithuania ~ Germany, data = diffEZ_HICPwideNoNA)
summary(fit2)
# a nicer way to display 
stargazer::stargazer(fit2,type = 'text')

# add to the plot
abline(fit2, col = "blue")

model_residuals2 = fit2$residuals
hist(model_residuals2)

# Plot the residuals
qqnorm(model_residuals2)
# Plot the Q-Q line
# plotting them will give a straight line if they are normally distributed
qqline(model_residuals2)
# notice there are big difference to the previous plotting of residuals 

# ggplot2 graphics
p_scatterDiff <-
  ggplot(data = diffEZ_HICPwideNoNA, mapping = aes(x = Germany, y = Lithuania)) +
  geom_point() +
  labs(title = '12 mth differences of annual HICP, Lithuania vs. Germany', x = 'Germany', y = 'Lithuania') +
  geom_smooth(method = lm, se = TRUE)

p_scatterDiff

p_scatterDiff + labs(subtitle = paste("Adj R2 = ",signif(summary(fit2)$adj.r.squared, 3),
                                  "Intercept =",signif(fit2$coef[[1]],3 ),
                                  " Slope =",signif(fit2$coef[[2]], 3),
                                  " P =",signif(summary(fit2)$coef[2,4], 3)))

# what about comparing more than one different relationships at once?
# in base graphics it is easy - assuming you get the axis limits correct
plot(diffEZ_HICPwideNoNA$Germany,diffEZ_HICPwideNoNA$France, type = 'p', col="red")
fit3 <- lm(France ~ Germany, data = diffEZ_HICPwideNoNA)
abline(fit3, col = "red")
points(diffEZ_HICPwideNoNA$Germany,diffEZ_HICPwideNoNA$Spain, col = 'green')
fit4 <- lm(Spain ~ Germany, data = diffEZ_HICPwideNoNA)
abline(fit4, col = "green")
points(diffEZ_HICPwideNoNA$Germany,diffEZ_HICPwideNoNA$Italy, col = 'blue')
fit4 <- lm(Italy ~ Germany, data = diffEZ_HICPwideNoNA)
abline(fit4, col = "blue")

# or better, show individually in a matrix
pairs(~Germany + France + Spain, ,data = diffEZ_HICPwideNoNA, main = 'Regressions of HICP, major countries')

# 'cars' package = Companion to Applied Regression, offers enhanced graphics for regressions
# install.packages("car")
library(car)
car::scatterplotMatrix(~Germany + France + Spain, ,data = diffEZ_HICPwideNoNA, main = 'Regressions of HICP, major countries')

# ggplot2 needs a bit of data wrangling to get the right shaped data to plot multiple regressions
# but like all problems in R, this is relatively easily solved with tidyverse functions

multichoiceHICP <-
  diffEZ_HICPwideNoNA[,c("Date",srs_chosen)] |>
  tidyr::pivot_longer(c(-Date), names_to = 'key', values_to = 'y')|>
  dplyr::filter(key %in% c("France","Spain","Italy"))

multichoiceHICP <- dplyr::left_join(multichoiceHICP, diffEZ_HICPwideNoNA[,c("Date","Germany")], by = 'Date')

# then it is simple
library(ggplot2)
ggplot(data = multichoiceHICP, aes(x = Germany, y = y, color = key, )) +
  geom_point() +
  facet_wrap(~ key, scales = "fixed")+
  geom_smooth(method = lm, se = TRUE)


