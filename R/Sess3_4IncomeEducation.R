## ---------------------------
##
## Script name: IncomeEducation.R
## Purpose of script: download Income by Educational Attainment from FRED and create a set of charts
## Author: Meyrick Chapman
## Date Created: Mon Mar 11 2024
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
source("R/ggstdplots.R")

lngdf <- function(df) {
longdf <-
df |>
tidyr::pivot_longer(c(-Date), names_to = 'key', values_to = 'value')
return(longdf)
}


ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes(x = fit$model[2], y = fit$model[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit1)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

## ---------------------------

API_key<-'39889bdd8fa77d15b0ae08ab03f7020c'
fredr_set_key(API_key)
startdate <- as.Date("2000-01-01")

CodeMap <- data.frame("SeriesID"= c("LEU0252919100Q","LEU0252917300Q",	"LEU0252916700Q"),
                      "SeriesName" = c("Bachelors degree","High School diploma only", "Less than High School Diploma"))

# fetch the data from FRED
# first series
EdAttainIncome <- alfred::get_fred_series(CodeMap[1,1], series_name = CodeMap[1,2], observation_start = startdate)

# then loop for other series, starting with second row
for(srs in 2:nrow(CodeMap)) {
  df <-
    alfred::get_fred_series(CodeMap[srs, 1],
                            series_name = CodeMap[srs, 2],
                            observation_start = startdate)
  EdAttainIncome <- cbind(EdAttainIncome, df[, 2])
  colnames(EdAttainIncome)[srs + 1] <- CodeMap[srs, 2]
}

colnames(EdAttainIncome)[1] <- "Date"



# store the data locally
readr::write_csv(EdAttainIncome,"Data/IncomeEducationalAttainment.csv")

# look at summary of data
summary(EdAttainIncome)

# plot data
lngEdAttain <- lngdf(EdAttainIncome)

p_EdAttain2 <- ggstandard(lngEdAttain,"Educational Attainment & Income","Source:  U.S. Bureau of Labor Statistics, HedgeAnalytics","5 years","US$ weekly")
p_EdAttain2
p_EdAttain2 + darktheme + theme(legend.position = 'bottom') + scale_color_HA_qualitative()

fit1 <- lm(`Bachelors degree` ~ `Less than High School Diploma`, data = EdAttainIncome)

summary(fit1)
stargazer::stargazer(fit1,type = 'text')

p_EdAttainLM <- ggplot(EdAttainIncome, aes(x = `Bachelors degree`, y = `Less than High School Diploma`)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

charttitle <- "Mapping Income: Bachelors degree to High School Diploma"
chartcaption <- "Source: U.S. Bureau of Labor Statistics"

p_EdAttainLM + labs(title = charttitle,
                    caption = chartcaption,
                    subtitle = paste("Adj R2 = ",signif(summary(fit1)$adj.r.squared, 5),
                     "Intercept =",signif(fit1$coef[[1]],5 ),
                     " Slope =",signif(fit1$coef[[2]], 5),
                     " P =",signif(summary(fit1)$coef[2,4], 5)))


