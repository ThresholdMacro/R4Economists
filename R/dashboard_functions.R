## ---------------------------
##
## Script name: dashboard_functions.R
## Purpose of script: collection of functions called by dashboard Rmd
## Author: Meyrick Chapman
## Date Created: Tue Feb 13 2024
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
plyr,
dplyr,
lubridate, 
ggthemes,
sysfonts,
showtext,
readr,
highcharter,
glue)


# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

## ---------------------------

t <- list(
  family = "sans serif",
  color = 'CCDDF3')

my_hc_theme <- highcharter::hc_theme_superheroes(
  chart = list(backgroundColor = "#1e3142"),
  xAxis = list(
    # tickColor = "#FFFFFF",
    labels = list(style = list(color = "#FFFFFF")),
    title = list(style = list(color = "#FFFFFF"))),
  yAxis = list(
    # tickColor = "#FFFFFF",
    labels = list(style = list(color = "#FFFFFF")),
    title = list(style = list(color = "#FFFFFF"))),
  title = list(style = list(color ="#CCDDF3",
                            fontFamily = "Arial",
                            fontSize = '18px'))
)

lngdf <- function(df) {
  longdf <-
    df |>
    tidyr::pivot_longer(c(-Date), names_to = 'key', values_to = 'value')
  return(longdf)
}

nearDate <- function(x, datevector) {
  nrdate <- which.min(abs(x-datevector))
  return(nrdate)
}

hchartHA <- function(lngdata, mktname, citation, yaxistitle) {
  colors <- c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c", "#2edaff")
  colors_dt <- data.frame(metric = unique(lngdata$key),
                          color = colors[1:length(unique(lngdata$key))])
  
  # Create the Highcharts chart
  hc <- highchart(type = "stock")
  metric_plot <- unique(lngdata$key)
  color_plot <- colors_dt |>
    dplyr::filter(metric %in% metric_plot) |>
    dplyr::pull(color)
  
  # name <- glue::glue("{metric_plot}")
  
  hc <- hc |>
    hc_add_series(
      data = lngdata,
      hcaes(x = as.Date(Date),
            group = key,
            y = value),
      color = color_plot,
      type = "line",
      # name = name,
      tooltip = list(pointFormat = "{series.name}: {point.y:.2f}")
    )
  
  hc <- hc |>
    hc_add_theme(my_hc_theme) |>
    hc_tooltip(valueDecimals = 3) |>
    hc_scrollbar(enabled = FALSE) |>
    hc_navigator(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_legend(enabled = TRUE) |>
    hc_rangeSelector(
      enabled = TRUE,
      labelStyle = list(color = "white"),
      inputStyle = list(color = "grey")
    ) |>
    hc_title(
      text = mktname,
      align = "left")|>
    hc_yAxis(title = list(text = yaxistitle),
             labels = list(format = "{value:.1f}")) |>
    hc_xAxis(title = list(text = "Date")) |>
    hc_caption(text = citation)
  
  return(hc)
}

hchartHAhist <- function(lngdata, ctitle,upperCI) {
  
   
  hc <-
    hchart(
      lngdata,
      type = "column",
      hcaes(x = `X`, y = Y),
      colorByPoint = FALSE,
      name = ctitle,
      tooltip = list(pointFormat = "{series.name}: {point.y:.2f}")
    ) |>
    hc_xAxis(title = list(text = "Months lead/lag")) |>
    hc_yAxis(
      title = list(text = "Correlation by month lead (-) or lag (+)"),
      labels = list(format = "{value:.2f}%"),
      plotLines = list(list(value = upperCI, color = "lightblue", width = 1.5,
                            dashStyle = "shortdash"),
                       list(value = -upperCI, color = "lightblue", width = 1.5,
                            dashStyle = "shortdash"))) |>
    hc_add_theme(my_hc_theme) |>
    hc_caption(text = "Source: HedgeAnalytics") 
  
  return(hc)
}


longFRED <- function(seriesID,seriesName, start_date) {
  df <- alfred::get_fred_series(seriesID, series_name = seriesName, observation_start = start_date)
  colnames(df)[1] <- 'Date'
  longfilename <- paste0("Data/Long",seriesName,".csv")
  old <- readr::read_csv(longfilename)
  df <- rbind(old,df)
  return(df)
}