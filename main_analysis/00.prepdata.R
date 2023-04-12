################################################################################
# Code for the analysis in:
#
#   "The mortality burden of extreme heat in Connecticut: 
#   a time series analysis"
#   Emily Goddard, Chengyi Lin, Yiqun Ma, and Kai Chen
#
# Updated: 12 April 2023
#   this code is also available at 
#   https://github.com/EmilyAlice10/mortality_extremeheat_CT
################################################################################

################################################################################
# PREPARE THE DATA
################################################################################

# LOAD PACKAGES
library(tidyverse); library(colorRamps); library(tsModel); library(lubridate);
library(splines); library(gam); library(mvmeta);  library(dlnm); library(MASS); 
library(tis); 

# CHECK VERSION OF PACKAGE
if(packageVersion("dlnm")<"2.2.0")
  stop("update dlnm package to version >= 2.2.0")

# LOAD THE DATASETS
CTmortality <- read.csv("CTmortality.csv")

# REFORMAT DATES AND SEASONS
CTmortality$date <- lubridate::ymd(CTmortality$date)
CTmortality$year <- lubridate::year(CTmortality$date)
CTmortality$month <- lubridate::month(CTmortality$date)
CTmortality$day <- lubridate::day(CTmortality$date)
CTmortality$yday <- lubridate::yday(CTmortality$date)
CTmortality$dow <- lubridate::wday(CTmortality$date, label=T)
CTmortality$dow <- as.factor(CTmortality$dow)
CTmortality$holiday <- tis::isHoliday(CTmortality$date, board = T)
CTmortality$holiday <- as.factor(CTmortality$holiday)

# SUBSET TO WARM MONTHS
CTmortality <- subset(CTmortality, month>=5 & month <=9)

################################################################################
# NB: RE-ASSIGN SPECIFICATIONS

# ASSIGN MORTALITY VALUE
CTmortality$Total <- CTmortality$Total 

# SET SPECIFICATION OF THE EXPOSURE FUNCTION
varfun = "ns"
dfarg = 4
varper <- c(10,75,90)

# SET SPECIFICATION OF THE LAG FUNCTION
lag = 0 
lagfun = "integer"

# DEGREE OF FREEDOM FOR SEASONALITY
dfseas <- 3

# DEGREE OF FREEDOM FOR HUMIDITY
dfhum <- 3

# MODEL FORMULA
formula <- Total~cb+dow+ns(date,df=dfseas*length(unique(year)))+holiday

# NAME TEMPERATURE PERCENTILES FOR EXTREME HEAT
exheatperc <- 90
exexheatperc <- 99

#