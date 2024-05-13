## ---------------------------
##
## Script name: DBNomics_example.R
## Purpose of script: download example timeseries from dbnomics.world
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

## load up the packages we will need: 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(plyr,
zoo,
tidyverse,
ggthemes,
sysfonts,
showtext,
rdbnomics)


## ---------------------------
source("R/ggstdplots.R")

lngdf <- function(df) {
longdf <-
df |>
tidyr::pivot_longer(c(-Date), names_to = 'key', values_to = 'value')
return(longdf)
}


## ---------------------------
# startdate set for beginning of Eurozone 
startdate <- as.Date("1999-01-01")

#  ---------------------------
# Using data from the internet is essential
# db.nomics.world. is a very good economic database to become familiar with.  
# URL: https://db.nomics.world/
# It is so useful because:
# a) it collects huge amounts of data from a very large set of providers worldwide (IMF, Eurostat, ONS, WorldBank)
# b) it arranges all these different data into a common format
# this means you can work with data from multiple sources much more easily using the same routines
# There are other very useful databases. 
# FRED (Federal Reserve Economic Database) hosted by St Louis Federal Reserve is very useful
# FRED needs its own API functions in the packages alfredr and fredr
# You need an API key for FRED. This is free and allows for recursive/multiple calls to the database. 

# ----------------------------
# get the data from db.nomics.world
# this is the link to the data we are going to get on their website:
# https://db.nomics.world/ECB/BSI?q=M.U2.N.V.M30.X.1.U2.2300.Z01.E
# 
# ----------------------------

EU_M3 <- rdbnomics::rdb(provider_code = "ECB",dataset_code = "BSI", ids = "M.U2.N.V.M30.X.1.U2.2300.Z01.E")

# ----------------------------
# use dplyr & lubridate to manipulate data - both parts of the tidyverse collection of packages
# the tidyverse standardises the treatment of data in a manner that restricts errors, and makes things clearer (tidier)
# it has been said that if you want to do anything serious data analysis you need to know dplyr 
# that's an overstatement, but it really helps A LOT 
# especially when used with pipe command (either %>% or |>) which is part of the tidyverse
# R has now included the pipe command into its base language
# ----------------------------

# one way to use the dplyr package
# note the object (data) which is being manipulated is always the first parameter
# this is a common feature of the tidyverse set of packages

dplyr::select(EU_M3, period,value)
dplyr::filter(EU_M3, period>startdate)

# we can 'pipe' the data object and the results through different functions using "%>%" (tidyverse) or "|>" (native R)
# this makes reading the code easier - 
# for instance, the following chunk of code is relatively easy to read as:
# "the new object "EU_M3sht' is created by taking the original "EU_M3' object, 
# selecting period and value columns and only including data after (greater than) startdate ("1999-01-01")

EU_M3sht <-
  EU_M3 |>
  dplyr::select(period, value) |>
  dplyr::filter(period >= startdate)

# all time series functions I have created use a column named 'Date' for dates, 
# this data uses 'period'
# so it is helpful to change the name so we can run functions used for other data on this
# also rename the value column to something more meaningful 
colnames(EU_M3sht) <- c( "Date", "BroadMoney EZ M3")

# change the date of each month to a more logical eom date
EU_M3sht$Date <- lubridate::ceiling_date(EU_M3sht$Date, unit = 'months')-1
EU_M3sht$`BroadMoney EZ M3` <- EU_M3sht$`BroadMoney EZ M3`/1000


# ----------------------------
# plotting the data

# ----------------------------
# plot the result - base R graphics
# ----------------------------

plot(EU_M3sht$Date,
     EU_M3sht$`BroadMoney EZ M3`,
     type = 'l',
     main = "Eurozone M3 since 1999, level, EUR bln",
     xlab = 'Date',
     ylab = 'EUR bln')


# ----------------------------
# tidyr function to arrange data into long format used by ggplot2
# ----------------------------
lngEU_M3 <-
  EU_M3sht |>
  tidyr::pivot_longer(c(-Date),names_to = 'key', values_to = 'value')


# ----------------------------
# plot the result - ggplot2
# ----------------------------

ggplot(data = lngEU_M3, aes(x = Date, y = value, color = key)) +
  geom_line() +
  labs(title = "Eurozone M3 since 1999, level, EUR bln")

# ----------------------------
# user customised plotting
# ----------------------------

p_EU_M3 <- ggstandard(lngEU_M3,"Eurozone M3 since 1999, level, EUR bln", "Source: db.nomics", "5 years","EUR bln")
p_EU_M3
p_EU_M3 + darktheme + theme(legend.position = 'bottom') + scale_color_HA_qualitative()


