## ---------------------------
##
## Script name: openXLS.R
## Purpose of script:
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
readxl)

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
source("~/Library/Mobile Documents/com~apple~CloudDocs/Meyrick/R/Libs/HAgraphics/R/ggstdplots.R")


## ---------------------------

PBoC <- readxl::read_excel("Data/PBoCBS.xlsx")
