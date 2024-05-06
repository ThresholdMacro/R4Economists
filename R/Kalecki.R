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
alfred)

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
source("~/Library/Mobile Documents/com~apple~CloudDocs/Meyrick/R/Libs/HAgraphics/R/ggstdplots.R")

API_key<-'39889bdd8fa77d15b0ae08ab03f7020c'
fredr_set_key(API_key)


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

KaleckiPctGDP <- data.frame("Date"= Kaleckidata$Date,Kaleckidata[,-1]/Kaleckidata$`Nominal GDP`)


readr::write_csv(Kaleckidata, "Data/Kaleckidata.csv")
readr::write_csv(KaleckiPctGDP, "Data/KaleckiPctGDP.csv")

# zKaleckiPctGDP[,c(2:ncol(KaleckiPctGDP))] <- 
#    sapply(KaleckiPctGDP[, c(2:ncol(KaleckiPctGDP))], scale, center = FALSE, scale = TRUE)
# 
# yrchg <- 4
# 
# chgKaleckiGDP <- KaleckiPctGDP[-c(1:(yrchg)),]
# chgKaleckiGDP[,c(2:ncol(chgKaleckiGDP))] <- 
#   sapply(KaleckiPctGDP[,c(2:ncol(KaleckiPctGDP))],diff,yrchg)
# 
# lngKaleckiGDP <-
#   KaleckiPctGDP[,!names(KaleckiPctGDP) %in% c("Nominal.GDP",
#                                               "Gross.Domestic.Investment",
#                                               "Consumption.of.Fixed.Capital",
#                                               "Corporate.Profits",
#                                               "Dividends",
#                                               "Personal.Savings",
#                                               "Net.Investment")] |>
#   pivot_longer(c(-Date), names_to = 'key', values_to = 'value')
# 
# 
# # lngKaleckiGDP <-
# #   KaleckiPctGDP[,names(KaleckiPctGDP) %in% c("Date","Net.Investment",
# #                                              "Gross.Domestic.Investment",
# #                                               "Corporate.Profits",
# #                                              "Foreign.Saving")] |>
# #   pivot_longer(c(-Date), names_to = 'key', values_to = 'value')
# # 
# # lngzKaleckiGDP<-
# #   zKaleckiPctGDP[,!names(zKaleckiPctGDP) %in% c("Nominal GDP",
# #                                               "Gross Domestic Investment",
# #                                               "Consumption of Fixed Capital")] |>
# #   pivot_longer(c(-Date), names_to = 'key', values_to = 'value')
# 
# 
# p_KaleckiPctGDP <-
#   ggstdpct(lngKaleckiGDP, "Selected Kalecki identities since 1947", "Source: FRED, HedgeAnalytics","10 years", "% of GDP")
# 
# p_KaleckiPctGDP + darktheme + theme(legend.position = 'bottom')
# 
# ccf(
#   KaleckiPctGDP$Net.Govt.Saving[217:312],
#   KaleckiPctGDP$Foreign.Saving[217:312],
#   lag.max = 20,
#   main = 'Budget Deficit vs CA Deficit',
#   xlab = 'Lead(-)/Lag(+) in quarters'
# )
# ccf(
#   KaleckiPctGDP$Corporate.Profits[217:312],
#   KaleckiPctGDP$Foreign.Saving[217:312],
#   lag.max = 20,
#   main = 'Corporate Profits vs CA Deficit',
#   xlab = 'Lead(-)/Lag(+) in quarters'
# )
# ccf(
#   KaleckiPctGDP$Net.Investment[217:312],
#   KaleckiPctGDP$Foreign.Saving[217:312],
#   lag.max = 20,
#   main = 'Net Investment vs CA Deficit',
#   xlab = 'Lead(-)/Lag(+) in quarters'
# )
# 
# ccf(
#   KaleckiPctGDP$Consumption.of.Fixed.Capital[217:312],
#   KaleckiPctGDP$Foreign.Saving[217:312],
#   lag.max = 20,
#   main = 'Consumption of Fixed Investment vs CA Deficit',
#   xlab = 'Lead(-)/Lag(+) in quarters'
# )
# 
# ccf(
#   chgKaleckiGDP$Net.Govt.Saving[217:304],
#   chgKaleckiGDP$Foreign.Saving[217:304],
#   lag.max = 20,
#   main = 'Budget Deficit vs CA Deficit, annual change',
#   xlab = 'Lead(-)/Lag(+) in quarters'
# )
# 
# id_used <- c("Personal.Savings","Corporate.Profits","Statistical.Discrepancy" )
# 
# modelGovtDeficit <- lm(KaleckiPctGDP$Net.Govt.Saving ~ ., data = KaleckiPctGDP[,id_used])
# stargazer::stargazer(modelGovtDeficit, type = 'text' )
# 
# string1 <-" Government deficit as a function of: "
# string2 <- paste0(gsub("\\."," ",id_used), collapse = ", ")
# plottitle <- paste(string1,string2)
# 
# plot(
#   KaleckiPctGDP$Date[-nrow(KaleckiPctGDP)],
#   as.vector(modelGovtDeficit$fitted.values),
#   type = 'l',
#   main = plottitle,
#   xlab = 'Date',
#   ylab = "% of GDP",
#   ylim = c(-0.25,0.1)
# )
# lines(KaleckiPctGDP$Date[-nrow(KaleckiPctGDP)],
#       KaleckiPctGDP$Net.Govt.Saving[-nrow(KaleckiPctGDP)],
#       col = 'red')
# 
# modelGovtDeficitPctchg <- lm(chgKaleckiGDP$Net.Govt.Saving ~ ., data = chgKaleckiGDP[,id_used])
# stargazer::stargazer(modelGovtDeficitPctchg, type = 'text' )
# 
# plot(
#   chgKaleckiGDP$Date[-nrow(chgKaleckiGDP)],
#   as.vector(modelGovtDeficitPctchg$fitted.values),
#   type = 'l',
#   main = plottitle,
#   xlab = 'Date',
#   ylab = "4 quarter change, % of GDP"
# )
# lines(chgKaleckiGDP$Date[-nrow(chgKaleckiGDP)], chgKaleckiGDP$Net.Govt.Saving[-nrow(chgKaleckiGDP)], col='red')
# 
# model_residuals = modelGovtDeficitPctchg$residuals
# # Plot the residuals
# hist(model_residuals)
# qqnorm(model_residuals)
# # Plot the Q-Q line
# qqline(model_residuals)
# 
# ccf(
#   chgKaleckiGDP$Statistical.Discrepancy,
#   chgKaleckiGDP$Corporate.Profits,
#   na.action = na.omit,
#   main = 'Statistical Discrepancy vs Corporate Profits, annual change',
#   xlab = 'Lead(-)/Lag(+) in quarters'
# )
