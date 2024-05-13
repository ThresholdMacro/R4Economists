## ---------------------------
##
## Script name: CourseLibs.R
## Purpose of script: load all (or most) libraries needed for the course
## Author: Meyrick Chapman
## Date Created: Mon May 13 2024
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
pacman::p_load(plyr,
tidyverse,
ggthemes,
sysfonts,
showtext,
ggthemes,
scales,
lattice,
RColorBrewer,
zoo,
sysfonts,
showtext,
highcharter,
glue,
rdbnomics,
DT,
xts,
foreign,
stargazer,
sandwich,#Sandwich is for heteroskedasticity-robust and cluster-robust errors 
lmtest,#lmtest is for Breusch-Pagan test
AER,
alfred,
fredr,
flexdashboard,
shiny,
shinyWidgets,
bslib,
reshape)

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
# source("~/Library/Mobile Documents/com~apple~CloudDocs/Meyrick/R/Libs/HAgraphics/R/ggstdplots.R")


## ---------------------------