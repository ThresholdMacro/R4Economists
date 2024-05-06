## ---------------------------
##
## Script name: ggstdplots
## Purpose of script: collection of ggplot2 scripts for generic chart generation
## Author: Meyrick Chapman
## Date Created: 2021-10-21
## Copyright (c) Hedge Analytics Ltd, 2021
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
library(scales)
library(lattice)
library(RColorBrewer)
library(zoo)
library(sysfonts)
library(showtext)

# library(pracma)
# source("functions/packages.R")       # loads up all the packages we need


## ---------------------------

## load up our functions into memory
# source("~/Library/Mobile Documents/com~apple~CloudDocs/Meyrick/R/Scripts/ggstdplots.R")


rescale.many <- function(dat, column.nos) {
  nms <- names(dat)
  for (col in column.nos) {
    name <- paste(nms[col], ".rescaled", sep = "")
    dat[name] <- rescale(dat[, col], to = c(-5, 5))
  }
  cat(paste("Rescaled ", length(column.nos),      " variable(s)n"))
  dat
}
## define line colours

HA_yellow <- "#f1c40f"
HA_green  <- "#2ecc71"
HA_red <- "#e00709"
HA_purple   <-  "#9b59b6"
HA_blue <- "#1e75d4"

scale_color_HA_qualitative <- function(color1 = HA_yellow, color2= HA_red, color3 = HA_green, color4 = HA_purple, color5 = HA_blue){
  scale_color_manual(values = c(color1,
                                color2,
                                color3,
                                color4,
                                color5))
}

scale_color_HA_sequential <- function(low_color = HA_red,
                                      high_color = HA_yellow) {
  scale_fill_gradient(low = low_color,
                      high = high_color)
}

DARK <- TRUE
if(DARK==TRUE){
  dark <- data.frame(bkrd='#1e3142',lne='#4c617d',zeroline='#3b4d5b', title='#FFFFFF', strip='#2ecc71')
} else{
  dark <- data.frame(bkrd='lightblue',lne='#FFFFFF',zeroline='darkgray', title='black', strip = 'gray')
}
font_add_google("Lato")
showtext_auto()

darktheme <-  theme(
  axis.text.x = element_text(angle = 0, hjust = 1,size=8),
  panel.background = element_rect(fill = dark$bkrd, colour = dark$bkrd),
  plot.background = element_rect(fill = dark$bkrd),
  text = element_text(family = "Lato"),
  title = element_text(colour = '#FFFFFF'),
  plot.caption = element_text(colour = '#4c617d', hjust = 0),
  axis.text = element_text(colour = '#FFFFFF', size=8),
  axis.title.x = element_blank(),
  panel.grid.minor =   element_blank(),
  panel.grid.major =   element_line(colour = dark$lne,linewidth = 0.15, linetype = 'dashed'),
  legend.background = element_rect(fill = dark$bkrd),
  legend.key=element_blank(),
  legend.text=element_text(color='#FFFFFF',size=8),
  legend.position=""
)

## ---------------------------
#' Standard time series chart using ggplot2
#'
#' Draw basic time series chart in standard format
#' @param Long_data A timeseries in long dataframe  column 1 Date ymd format, column key holds names of series column value the timeseries values
#' @param charttitle Title of the chart in inverted commas
#' @param chartcaption Caption, usually showing source of data
#' @param xaxis_date_breaks How long between tick marks on x-axis
#' @param yaxis_title Text to show on y-axis
#' @return a ggplot2 chart object
#' @export
ggstandard<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
  ggplot(data = Long_data,
         aes(
           x = as.Date(Date),
           y = value,
           colour = key
         )) +
    geom_line() +
    labs(title = charttitle,
         caption = chartcaption) +
    scale_x_date(date_breaks = xaxis_date_breaks,
                 date_labels =  "%b %y") +
    scale_y_continuous(name = yaxis_title, labels = scales::comma)+
    theme(legend.position="right")+
    tidyquant::theme_tq()
}

#' Standard time series barchart using ggplot2
#'
#' Draw basic time series chart in standard format
#' @param Long_data A timeseries in long dataframe  column 1 Date ymd format, column key holds names of series column value the timeseries values
#' @param charttitle Title of the chart in inverted commas
#' @param chartcaption Caption, usually showing source of data
#' @param xaxis_date_breaks How long between tick marks on x-axis
#' @param yaxis_title Text to show on y-axis
#' @return a ggplot2 chart object
#' @export

ggstdbar<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
  ggplot(data = Long_data,
         aes(
           x = as.Date(Date),
           y = value,
           colour = key,
           fill= key
         )) +
    geom_bar(position='dodge', stat='identity') +
    labs(title = charttitle,
         caption = chartcaption) +
    scale_x_date(date_breaks = xaxis_date_breaks,
                 date_labels =  "%b %y",
                 name = "Date") +
    scale_y_continuous(name = yaxis_title, labels = scales::comma)+
    theme(legend.position="right")+
    tidyquant::theme_tq()
}
#' Standard time series returning % on y-axis using ggplot2
#'
#' Draw basic time series chart in standard format using percent format on y-axis
#' @param Long_data A timeseries in long dataframe  column 1 Date ymd format, column key holds names of series column value the timeseries values
#' @param charttitle Title of the chart in inverted commas
#' @param chartcaption Caption, usually showing source of data
#' @param xaxis_date_breaks How long between tick marks on x-axis
#' @param yaxis_title Text to show on y-axis
#' @return a ggplot2 chart object
#' @export

ggstdpct<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
  ggplot(data = Long_data,
         aes(
           x = as.Date(Date),
           y = value,
           colour = key
         )) +
    geom_line() +
    labs(title = charttitle,
         caption = chartcaption) +
    scale_x_date(date_breaks = xaxis_date_breaks,
                 date_labels =  "%b %y",
                 name = "Date") +
    scale_y_continuous(name = yaxis_title, labels = scales::percent)+
    theme(legend.position="bottom")+
    tidyquant::theme_tq()
}

#' Standard time series column chart using ggplot2
#'
#' Draw basic time series column chart in standard format
#' @param Long_data A timeseries in long dataframe  column 1 Date ymd format, column key holds names of series column value the timeseries values
#' @param charttitle Title of the chart in inverted commas
#' @param chartcaption Caption, usually showing source of data
#' @param xaxis_date_breaks How long between tick marks on x-axis
#' @param yaxis_title Text to show on y-axis
#' @return a ggplot2 chart object
#' @export

ggstdcol<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
  ggplot(data = Long_data,
         aes(
           x = as.Date(Date),
           y = value)) +
    geom_col(fill = 'steelblue') +
    labs(title = charttitle,
         caption = chartcaption) +
    scale_x_date(date_breaks = xaxis_date_breaks,
                 date_labels =  "%b %y",
                 name = "Date") +
    scale_y_continuous(name = yaxis_title, labels = scales::percent)+
    theme(legend.position="bottom")+
    tidyquant::theme_tq()
}

#' Replicate PerformanceAnalytics chart function using ggplot2
#'
#' Draw basic time series column chart in standard format
#' @param rtn.obj A timeseries of market returns
#' @param main Title of the chart in inverted commas
#' @param xaxis_date_breaks How long between tick marks on x-axis
#' @return a ggplot2 chart object
#' @export
#'
gg.charts.PerformanceSummary <- function(rtn.obj, geometric = TRUE, main = "", xaxis_date_breaks = "6 months",plot = TRUE)
{
    # load libraries
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  suppressPackageStartupMessages(require(reshape))
  suppressPackageStartupMessages(require(PerformanceAnalytics))

  # create function to clean returns if having NAs in data
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)]<- na.replace
    univ.rtn.xts.obj
  }

  # Create cumulative return function
  cum.rtn <- function(clean.xts.obj, g = TRUE)
  {
    x <- clean.xts.obj
    if(g == TRUE){y <- cumprod(x+1)-1} else {y <- cumsum(x)}
    y
  }

  # Create function to calculate drawdowns
  dd.xts <- function(clean.xts.obj, g = TRUE)
  {
    x <- clean.xts.obj
    if(g == TRUE){y <- PerformanceAnalytics:::Drawdowns(x)} else {y <- PerformanceAnalytics:::Drawdowns(x,geometric = FALSE)}
    y
  }

  # create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
  cps.df <- function(xts.obj,geometric)
  {
    x <- clean.rtn.xts(xts.obj)
    series.name <- colnames(xts.obj)[1]
    tmp <- cum.rtn(x,geometric)
    # tmp$rtn <- x
    tmp$dd <- dd.xts(x,geometric)
    colnames(tmp) <- c("Cumulative Return","Drawdown") # names with space
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$Date <- as.POSIXct(index(tmp))
    tmp.df.long <- melt(tmp.df,id.var="Date")
    tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }

  # A conditional statement altering the plot according to the number of assets
  if(ncol(rtn.obj)==1)
  {
    # using the cps.df function
    df <- cps.df(rtn.obj,geometric)
    # adding in a title string if need be
    if(main == ""){
      title.string <- paste("Asset Performance")
    } else {
      title.string <- main
    }

    gg.xts <- ggplot(df, aes_string( x = "Date", y = "value", group = "variable" )) +
      facet_grid(variable ~ ., scales = "free_y", space = "free_y") +
      geom_line(data = subset(df, variable == "Cumulative Return"), colour = dark$lne) +
      # geom_bar(data = subset(df, variable == "Return"), stat = "identity") +
      geom_line(data = subset(df, variable == "Drawdown"), colour = dark$lne) +
      geom_hline(yintercept = 0, linewidth = 0.5, colour = dark$zeroline) +
      ggtitle(title.string) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 1),
        panel.background = element_rect(fill = dark$bkrd, colour = dark$bkrd),
        strip.background = element_rect(fill = dark$lne),
        plot.background = element_rect(fill = dark$bkrd),
        title = element_text(colour = dark$lne),
        axis.text = element_text(colour = dark$lne)
      ) +
      scale_x_datetime(breaks = date_breaks(width = xaxis_date_breaks), labels = date_format("%m/%Y")) +
      ylab("") +
      xlab("")

  }
  else
  {
    # a few extra bits to deal with the added rtn columns
    no.of.assets <- ncol(rtn.obj)
    asset.names <- colnames(rtn.obj)
    df <- do.call(rbind,lapply(1:no.of.assets, function(x){cps.df(rtn.obj[,x],geometric)}))
    df$asset <- ordered(df$asset, levels=asset.names)
    if(main == ""){
      title.string <- paste("Asset",asset.names[1],asset.names[2],"Performance")
    } else {
      title.string <- main
    }

    if(no.of.assets>1){legend.rows <- 2} else {legend.rows <- no.of.assets}

    gg.xts <- ggplot(df, aes_string(x = "Date", y = "value" )) +

      # panel layout
      facet_grid(variable~., scales = "free_y", space = "free_y", shrink = TRUE, drop = TRUE, margin =
                   , labeller = label_value) + # label_value is default

      # display points for Index and Drawdown, but not for Return
     # geom_point(data = subset(df, variable == c("Cumulative Return","Drawdown"))
      #           , aes(colour = factor(asset), shape = factor(asset)), linewidth = 1.2, show.legend = TRUE) +

      # manually select shape of geom_point
      scale_shape_manual(values = c(1,2,3)) +

      # line colours for the Index
      geom_line(data = subset(df, variable == "Cumulative Return"), aes(colour = factor(asset)), show.legend = FALSE) +

      # bar colours for the Return
      # geom_bar(data = subset(df,variable == "Return"), stat = "identity"
               # , aes(fill = factor(asset), colour = factor(asset)), position = "dodge", show.legend = FALSE) +

      # line colours for the Drawdown
      geom_line(data = subset(df, variable == "Drawdown"), aes(colour = factor(asset)), show.legend = FALSE) +

      # horizontal line to indicate zero values
      geom_hline(yintercept = 0, linewidth = 0.5, colour = dark$zeroline) +

      # horizontal ticks
      scale_x_datetime(breaks = date_breaks(width = xaxis_date_breaks), labels = date_format("%m/%Y")) +

      # main y-axis title
      ylab("") +

      # main x-axis title
      xlab("") +

      # main chart title
      ggtitle(title.string)

    # legend

    #gglegend <- guide_legend(override.aes = list(size = 3))

    gg.xts <- gg.xts +
      scale_color_manual(values=c('orange', 'pink', 'red'))

      # gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
       # gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"

      theme( legend.key = element_rect(fill = dark$bkrd, colour = dark$bkrd)
             , axis.text.x = element_text(angle = 0, hjust = 1)
             , strip.background = element_rect(fill = dark$bkrd)
             , panel.background = element_rect(fill = dark$bkrd, colour = dark$bkrd)
             , panel.grid.major = element_line(colour = dark$zeroline, linewidth = 0.5)
             , panel.grid.minor = element_line(colour = NA, linewidth = 0.0)
             , plot.background = element_rect(fill = dark$bkrd)
             , title = element_text(colour = dark$lne)
             , axis.text = element_text(colour = dark$lne)
      )

  }

  assign("gg.xts", gg.xts,envir=.GlobalEnv)
  if(plot == TRUE){
    plot(gg.xts)
  } else {}

}



#Customized labels can be obtained with the following function:
#create a function to store fancy axis labels

my_labeller <- function(var, value){ # from the R Cookbook
    value <- as.character(value)
    if (var=="variable")
    {
      value[value=="Index"] <- "Cumulative Returns"
      value[value=="Return"] <- "Daily Returns"
      value[value=="Drawdown"] <- "Drawdown"
    }
    return(value)
  }

# --------------------------------------
#' Create a quadrant chart showing change over 6mth period and change in change
#'
#' Draw quadrant chart
#' @param data A dataframe timeseries of format column1  Date column2 timeseries of change.
#' @return a ggplot2 chart object
#' @export
#'
quadgraph <- function(data) {

  df1 <- data[,c(1,2)]

  seriesname <- names(df1[2])
  seriesname <- gsub("."," ", seriesname, fixed = T)
  startdate <- year(min(df1[,1]))
  enddate <- max(df1[,1])

  df1 <-
    data.frame(
      'period' = df1[,1],
      'Indicator' = as.vector(df1[, 2]),
      'Indicator_6m_chg' = (df1[, 2] - lag(df1[, 2], 6))
    )

  df1 <- rescale.many(df1, c(2:3))
  df1 <-
    df1 %>%
    filter(period >= as.Date(Sys.Date()) %m-% months(36))

  tmp_date <- df1 %>% sample_frac(0.3)
  myPalette <- colorRampPalette(rev(brewer.pal(6, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(min(df1$period), max(df1$period)))

  p <-
    ggplot(df1,
           aes(`Indicator.rescaled`, `Indicator_6m_chg.rescaled`, color = period)) + sc
  p <- p + scale_x_continuous(expand = c(0, 0), limits = c(-5.5, 5.5))
  p <- p + scale_y_continuous(expand = c(0, 0), limits = c(-5.5, 5.5))


  p <- p + labs(x = paste0("Level: min/max since ",startdate), y = paste0("6mth change: min/max since ",startdate))
  p <-
    p + theme(axis.title.x = element_text(
      hjust = 0,
      vjust = 4,
      colour = "black",
      size = 12,
      face = "bold"
    ))
  p <-
    p + theme(axis.title.y = element_text(
      hjust = 0,
      vjust = 0,
      colour = "black",
      size = 12,
      face = "bold"
    ))

  p <- p + theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

  p <-
    p + ggtitle(paste0(
      seriesname,
      " ",
      as.yearmon(min(df1$period)),
      " to ",
      enddate
    ))

  p <- p +
    annotate(
      "rect",
      xmin = 0,
      xmax = 5.5,
      ymin = 0,
      ymax = 5.5,
      fill = "#ebf7f7"
    )  +
    annotate(
      "rect",
      xmin = -5.5,
      xmax = 0,
      ymin = -5.5,
      ymax = 0 ,
      fill = "#ebe9e8"
    ) +
    annotate(
      "rect",
      xmin = 0,
      xmax = 5.5,
      ymin = 0,
      ymax = -5.5,
      fill = "#FFFFFF"
    ) +
    annotate(
      "rect",
      xmin = -5.5,
      xmax = 0,
      ymin = 0,
      ymax = 5.5,
      fill = "#FFFFFF"
    )

  p <-
    p + theme(panel.border = element_rect(
      colour = "lightgrey",
      fill = NA,
      linewidth = 4
    ))
  p <- p + geom_hline(yintercept = 0,
                      color = "lightgrey",
                      linewidth = 1.5)
  p <- p + geom_vline(xintercept = 0,
                      color = "lightgrey",
                      linewidth = 1.5)

  p <- p + geom_label(
    aes(x = -2, y = 4, label = "Upswing"),
    label.padding = unit(2, "mm"),
    fill = "grey",
    color = "#FFFFFF"
  )
  p <- p + geom_label(
    aes(x = 2, y = 4, label = "Expansion"),
    label.padding = unit(2, "mm"),
    fill = "grey",
    color = "#FFFFFF"
  )
  p <- p + geom_label(
    aes(x = -2, y = -4, label = "Contraction"),
    label.padding = unit(2, "mm"),
    fill = "grey",
    color = "#FFFFFF"
  )
  p <- p + geom_label(
    aes(x = 2, y = -4, label = "Downswing"),
    label.padding = unit(2, "mm"),
    fill = "grey",
    color = "#FFFFFF"
  )
  p <- p + geom_segment(aes(
    xend=c(tail(Indicator.rescaled, n=-1), NA),
    yend=c(tail(Indicator_6m_chg.rescaled, n=-1), NA)),
    arrow=arrow(length=unit(0.4,"cm")))+
    geom_point( size = 3)+
    theme(legend.position = "none")

  return(p)
}
