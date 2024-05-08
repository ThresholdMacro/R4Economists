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

startdate <- as.Date("2000-01-01")

EZ_HICP <- rdbnomics::rdb(provider_code = "Eurostat",dataset_code = "prc_hicp_manr", ids = "M.RCH_A.CP00..")
# ----------------------------
# use dplyr & lubridate to manipulate data
# ----------------------------

EZ_HICPsht <-
  EZ_HICP |>
  dplyr::select(period,`Geopolitical entity (reporting)`, value)|>
  dplyr::filter(period>=as.Date(startdate))

colnames(EZ_HICPsht) <- c( "Date", "Country","value")
EZ_HICPsht$Date <- lubridate::ceiling_date(EZ_HICPsht$Date, unit = 'months')-1

# ----------------------------
# built-in tidyr function to arrange data 
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

tail(EZ_HICPwide)

summary(EZ_HICPwide)

datatable(data= EZ_HICPwide)
# some issues with display, so let's get rid of pesky columns with very long names

datatable(data= EZ_HICPwide[,-c(11,15,18)])


# ============================
# 2. Visualise and analysing the data
# ============================

# ----------------------------
# visualise time series the simple way
# convert to time series object
# ----------------------------

# subset the large dataset for convenience
srs_chosen <- c('Germany','France','Italy','Spain')

ts_EZ_HICP <- xts::xts(EZ_HICPwide[,srs_chosen], order.by=as.Date(EZ_HICPwide$Date))
plot.xts(ts_EZ_HICP, auto.legend = TRUE, main = 'Headlinie HICP: major EZ countries')

xts::addLegend(legend.loc="topleft", legend.names=names(ts_EZ_HICP), 
               lty = 1, col=1:ncol(ts_EZ_HICP),text.col=1:ncol(ts_EZ_HICP), bg="white", bty=1)

# ----------------------------
# visualise a subset of the data using more modern, flexible approach (ggplot2)
# ----------------------------
# long format
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
sapply(EZ_HICPwide[,-c(11,15,18)], function(x) sum(is.na(x)))

# what to do? exclude? 
df <- tail(na.omit(EZ_HICPwide))
# oops! that excludes too much - blame Brexit! 
# exclude columns with NAs
EZ_HICPwide[ , apply(EZ_HICPwide, 2, function(x) !any(is.na(x)))]
EZ_HICPwide <- EZ_HICPwide[ , apply(EZ_HICPwide, 2, function(x) !any(is.na(x)))]

# ============================
# 4. Exploring numeric data attributes (Univariate analysis)
# ============================

# ----------------------------
# analyse a subset of the data
# ----------------------------

# ----------------------------
# 4.1 Measure the central tendency
# ----------------------------
# mean
mean(EZ_HICPwide$Germany)
median(EZ_HICPwide$Germany)

mode(EZ_HICPwide$Germany)
# oop! mode() tells what kind of structure the object is!
# create a function getmode() 

getmode <- function(v) {
  uniqv <- unique(v)
  # find the most frequent occurance using tabulate & max
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(EZ_HICPwide$Germany)

# ----------------------------
# 4.2 The shape of the distribution
# ----------------------------
# interquartile range (50% of data lies within this range around the mean)
IQR(EZ_HICPwide$Germany,na.rm=TRUE)

quantile(EZ_HICPwide$Germany, seq(from = 0, to = 1, by = 0.1))

var(na.omit(EZ_HICPwide$Germany))
sd(na.omit(EZ_HICPwide$Germany))

library(moments)
skewness(EZ_HICPwide$Germany)
# highly positively skewed

# histograms are useful
# simple
hist(EZ_HICPwide$Germany)

# less simple, but multiple histograms on same page
p_HICPscatter <- ggplot(data = selectHICP, aes(x = value)) + geom_histogram(binwidth = 0.5)
p_HICPscatter + facet_wrap(~Country)

# ----------------------------
# 4.3 Explore the spread using Visualization plots 
# 4.4 Identify outliers and unusual observations
# ----------------------------
# a boxplot to show 50% of occurances & mean
# simple

boxplot(value ~ Country, data = selectHICP)
title("HICP annual % change for selected countries")
# notice the order of countries has changed to alphabetical. We don't want that so need to reorder back to our original order
boxplot(value ~ fct_reorder(Country, value), data = selectHICP, xlab = 'Country')
title("HICP annual % change for selected countries")

# less simple, but flexible
ggplot(data = selectHICP, mapping = aes(x = Country, y = selectHICP$value)) +
  geom_boxplot()+ 
  labs(title = "HICP annual % change for selected countries",
       y = "Percent change 12 months")

# notice the order of the countries is alphabetical. Let's reorder back to original order using factor (fct_reorder)
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

ccf(EZ_HICPwide$Germany, EZ_HICPwide$Lithuania)


# ----------------------------
# 5.2 Visualization of relationship using scatter plot
# ----------------------------
# simple
plot(
  EZ_HICPwide$Germany,
  EZ_HICPwide$Lithuania,
  main = 'Scatter plot of HICP, Germany vs. Lithuania',
  xlab = 'Germany',
  ylab = 'Lithuania'
)
abline(lm(Lithuania ~ Germany, data = EZ_HICPwide), col = "blue")

# less simple
p_scatter <-
  ggplot(
  data = EZ_HICPwide,
  mapping = aes(x = Germany, y = Lithuania)
) +
  geom_point() +
  labs(title = 'Scatter plot of HICP, Germany vs. Lithuania', x = 'Germany', y = 'Lithuania') +
  geom_smooth(method = lm, se = TRUE)

p_scatter
