## ---------------------------
##
## Script name: Kalecki.R
## Purpose of script: download Kalecki identities from FRED
## Author: Meyrick Chapman
## Date Created: Mon Apr 29 2024
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
alfred,
fredr)

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
#source("~/Library/Mobile Documents/com~apple~CloudDocs/Meyrick/R/Libs/HAgraphics/R/ggstdplots.R")

API_key<-'39889bdd8fa77d15b0ae08ab03f7020c'
fredr::fredr_set_key(API_key)


## ---------------------------

CodeMap <- 
data.frame(
  "series_id" = c(
    "GDP",
    "CPATAX",
    "A030RC1Q027SBEA",
    "DIVIDEND",
    "NETFI",
    "PSAVE",
    "TGDEF",
    "COFC",
    "W170RC1Q027SBEA"
  ),
  "series_name" = c(
    "Nominal GDP",
    "Corporate Profits",
    "Statistical Discrepancy",
    "Dividends",
    "Foreign Saving",
    "Personal Savings",
    "Net Govt Saving",
    "Consumption of Fixed Capital",
    "Gross Domestic Investment"
  )
)

Kaleckidata <- alfred::get_fred_series(series_id = CodeMap$series_id[1], api_key = API_key)
colnames(Kaleckidata) <- c("Date", CodeMap$series_name[1])

for (row in 2:nrow(CodeMap)) {
  df <-
    alfred::get_fred_series(series_id = CodeMap$series_id[row], api_key = API_key)
    colnames(df) <- c("Date", CodeMap$series_name[row])
    Kaleckidata <- left_join(Kaleckidata,df, by = "Date")
}

Kaleckidata$`Net Investment` <- Kaleckidata$`Gross Domestic Investment`- Kaleckidata$`Consumption of Fixed Capital`
Kaleckidata$Date <- lubridate::ceiling_date(Kaleckidata$Date, unit = 'quarter')-1
Kaleckidata <- Kaleckidata |> filter(Date >= '1947-03-31')

readr::write_csv(Kaleckidata, "Data/Kaleckidata.csv")
