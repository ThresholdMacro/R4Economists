## ---------------------------
##
## Script name: DBNomics_example.R
## Purpose of script: download example timeseries from dbnomics 
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

startdate <- as.Date("2000-01-01")
# ----------------------------
# get the data from db.nomics.world
# ----------------------------

EU_M3 <- rdbnomics::rdb(provider_code = "ECB",dataset_code = "BSI", ids = "M.U2.N.V.M30.X.1.U2.2300.Z01.E")

# ----------------------------
# use dplyr & lubridate to manipulate data
# ----------------------------

EU_M3sht <-
  EU_M3 |>
  dplyr::select(period, value)|>
  dplyr::filter(period>=as.Date(startdate))

colnames(EU_M3sht) <- c( "Date", "BroadMoney EU M3")
EU_M3sht$Date <- lubridate::ceiling_date(EU_M3sht$Date, unit = 'months')-1
EU_M3sht$`BroadMoney EU M3` <- EU_M3sht$`BroadMoney EU M3`/1000

# ----------------------------
# tidyr function to arrange data 
# ----------------------------
lngEU_M3 <-
  EU_M3sht |>
  tidyr::pivot_longer(c(-Date),names_to = 'key', values_to = 'value')

# ----------------------------
# user defined function to arrange data - same output
# ----------------------------
lngEU_Ma <-
  lngdf(EU_M3sht)

# ----------------------------
# user customised plotting
# ----------------------------

p_EU_M3 <- ggstandard(lngEU_M3,"EU M3 since 2000, level, EUR bln", "Source: db.nomics", "5 years","EUR blns")
p_EU_M3 + darktheme + theme(legend.position = 'bottom') + scale_color_HA_qualitative()

