## ---------------------------
##
## Script name: LoanOfficersSurvey
## Purpose of script:
## Author: Meyrick Chapman
## Date Created: Wed May 10 2023
## Copyright (c) HedgeAnalytics Ltd, 2023
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

library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(readr)
library(alfred)
library(ecm)
library(scales)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
add.col <- function(df, new.col) {
  n.row <- dim(df)[1]
  length(new.col) <- n.row
  cbind(df, new.col)
}

rescale.many <- function(dat, column.nos) {
nms <- names(dat)
for (col in column.nos) {
name <- paste(nms[col], ".rescaled", sep = "")
dat[name] <- rescale(dat[, col], to = c(0, 5))
}
cat(paste("Rescaled ", length(column.nos),      " variable(s)n"))
dat
}

add_rec_shade<-function(st_date,ed_date,shade_color="darkgray")
{
  library(ecm)
  library(ggplot2)
  library(dplyr)
  recession<-readr::read_csv("Data/USrecession.csv")
  recession$date <- as.Date(ymd(recession$date))
  recession$diff<-recession$value-lagpad(recession$value,k=1)
  recession<-recession[!is.na(recession$diff),]
  recession.start<-recession[recession$diff==1,]$date
  recession.end<-recession[recession$diff==(-1),]$date
  if(length(recession.start)>length(recession.end))
  {recession.end<-c(recession.end,Sys.Date())}
  if(length(recession.end)>length(recession.start))
  {recession.start<-c(min(recession$date),recession.start)}
  recs<-as.data.frame(cbind(recession.start,recession.end))
  recs$recession.start<-
    as.Date(as.numeric(recs$recession.start),origin=as.Date("1970-01-01"))
  recs$recession.end<-as.Date(recs$recession.end,origin=as.Date("1970-01-01"))
  if(nrow(recs)>0)
  {
    rec_shade<-geom_rect(data=recs, inherit.aes=F, aes(xmin=recession.start,
                                                       xmax=recession.end, ymin=-Inf, ymax=+Inf), fill=shade_color,alpha=0.5)
    return(rec_shade)
  }
}

source("R/ggstdplots.R")

## ---------------------------

# API_key <- 'x'
# Code_map <-
#   data.frame(Series_ID = c(
#     'DRISCFLM',
#     'DRISCFS',
#     'DRTSCILM',
#     'DRTSCIS',
#     'DRSDCILM',
#     'DRSDCIS',
#     'SUBLPDRCDC',
#     'DEMCC',
#     'DEMOTHCONS',
#     'H0SUBLPDHMDNQ',
#     'DRSDPM',
#     'SUBLPDHMDENQ'),
#     Series_Name = c(
#       'Higher Spreads to Large/Medium Cos',
#       'Higher Spreads to Small Cos',
#       'Tighter Conds to Large/Medium Cos',
#       'Tighter Conds to Small Cos',
#       'Loan Demand: Large/Medium Cos',
#       'Loan Demand: Small Cos',
#       'Loan Demand: RE and Constr',
#       'Demand: Credit Card Loans',
#       'Demand: Cons Loans, excl Credit Cards/Autos',
#       'Demand: Mortgage Loans',
#       'Demand: Prime Mortgages',
#       'Demand: GSE-eligible mortgages'
#     ))
# 
# dat_csv <- data.frame("date"=as.Date(seq(from = as.POSIXct(as.Date("1990-04-01"), tz="GMT"),to = as.POSIXct(Sys.Date()), by='3 month')))
# 
# for(id in 1:nrow(Code_map)){
#   
#   tmp <- get_fred_series(series_id = Code_map[id,1], series_name = Code_map[id,2], observation_start = '1990-01-01', api_key = API_key)  
#   dat_csv <- left_join(dat_csv,tmp, by = 'date')
#   
# }    
# 
# colnames(dat_csv)[1] <- 'Date'
# 
# dat_csv$Mortgages <- rowSums(dat_csv[,c(11,12,13)],na.rm=T)
# 
# write_csv(dat_csv,"Data/LoanOfficersSurvey.csv")

dat_csv<-readr::read_csv("Data/LoanOfficersSurvey.csv")
loantype <- c("Mortgages")

p_loandemand <-
dat_csv[,c("Date",loantype)] |> 
  dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index") |> 
  ggplot(aes(x = Date, y = Index, color = Position)) + 
  geom_line() + 
  labs(title = "Loan Officers Survey")+
  theme(legend.position="bottom")

# add recesssion bars

p_loandemand +
  add_rec_shade(as.Date(min(dat_csv$Date)),as.Date(Sys.Date()))
  

mu <- dat_csv[,c(1:3)] |> 
  dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index")|>
  ddply("Position", summarise, grp.mean=mean(Index))

lst <-dat_csv[,c(1:3)] |> 
  dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index")|>
  ddply("Position", summarise, grp.mean=last(Index))

dat_csv[,c(1:3)] |> 
  dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index") |> 
  ggplot(aes(x = Index, color = Position)) + 
  geom_histogram(aes(y=..density..), fill="white", position="dodge", binwidth = 3)+
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(data=lst, aes(xintercept=grp.mean, color=Position),
             linetype="dashed") +
  labs(title = "Loan Officers Survey: distribution of responses since 1990, with current response (dotted verticals)", 
       subtitle = paste0("To: ",format(dat_csv$Date[nrow(dat_csv)],"%B %Y")), 
       caption = "source: Federal Reserve, HedgeAnalytics") +
  theme(legend.position="bottom")

mu <- dat_csv[,c(1,4,5)] |> 
  dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index")|>
  ddply("Position", summarise, grp.mean=mean(Index))

lst <-dat_csv[,c(1,4,5)] |> 
  dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index")|>
  ddply("Position", summarise, grp.mean=last(Index))

dat_csv[,c(1,4,5)] |> 
  dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index") |> 
  ggplot(aes(x = Index, color = Position)) + 
  geom_histogram(aes(y=..density..), fill="white", position="dodge", binwidth = 3)+
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(data=lst, aes(xintercept=grp.mean, color=Position),
             linetype="dashed") +
  labs(title = "Loan Officers Survey: distribution of responses since 1990, with current response (dotted verticals)", 
       subtitle = paste0("To: ",format(dat_csv$Date[nrow(dat_csv)],"%B %Y")), 
       caption = "source: Federal Reserve, HedgeAnalytics") +
  theme(legend.position="bottom")


mu <- dat_csv[,c(1,6,7)] |> 
  dplyr::filter(Date > as.Date("1991-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index")|>
  ddply("Position", summarise, grp.mean=mean(Index))

lst <-dat_csv[,c(1,6,7)] |> 
  dplyr::filter(Date > as.Date("1991-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index")|>
  ddply("Position", summarise, grp.mean=last(Index))

dat_csv[,c(1,6,7)] |> 
  dplyr::filter(Date > as.Date("1991-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index") |> 
  ggplot(aes(x = Index, color = Position)) + 
  geom_histogram(aes(y=..density..), fill="white", position="dodge", binwidth = 3)+
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(data=lst, aes(xintercept=grp.mean, color=Position),
             linetype="dashed") +
  labs(title = "Loan Officers Survey: distribution of responses since 1991, with current response (dotted verticals)", 
       subtitle = paste0("To: ",format(dat_csv$Date[nrow(dat_csv)],"%B %Y")), 
       caption = "source: Federal Reserve, HedgeAnalytics") +
  theme(legend.position="bottom")

mu <- dat_csv[,c(1,14)] |> 
  dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index")|>
  ddply("Position", summarise, grp.mean=mean(Index))

lst <-dat_csv[,c(1,14)] |> 
  dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index")|>
  ddply("Position", summarise, grp.mean=last(Index))

dat_csv[,c(1,14)] |> 
  dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index") |> 
  ggplot(aes(x = Index, color = Position)) + 
  geom_histogram(aes(y=..density..), fill="white", position="dodge", binwidth = 3)+
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(data=lst, aes(xintercept=grp.mean, color=Position),
             linetype="dashed") +
  labs(title = "Loan Officers Survey: distribution of responses since 1990, with current response (dotted verticals)", 
       subtitle = paste0("To: ",format(dat_csv$Date[nrow(dat_csv)],"%B %Y")), 
       caption = "source: Federal Reserve, HedgeAnalytics") +
  theme(legend.position="bottom")

dat_csv[,c(1,14)] |> 
  dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index") |> 
  ggplot(aes(x = Date, y = Index, color = Position)) + 
  geom_line() + 
  labs(title = "Loan Officers Survey: demand for mortgages")+
  theme(legend.position="bottom")

DFF <- get_fred_series(series_id = 'DFF', series_name = 'Fed Funds rate', observation_start = '1990-01-01')
colnames(DFF)[1] <- 'Date'
DFFmort <-
  left_join(dat_csv[,c(1,14)],DFF, by = 'Date')

DFFmort <-
rescale.many(DFFmort,2)

DFFmort[,c(1,2,3)] |>
dplyr::filter(Date > as.Date("1990-01-01")) |>
  tidyr::pivot_longer(-Date, names_to = "Position", values_to = "Index") |> 
  ggplot(aes(x = Date, y = Index, color = Position)) + 
  geom_line() + 
  labs(title = "Fed Funds & demand for mortgages")+
  theme(legend.position="bottom")


