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
# DEFINE THE REFERENCE TEMPERATURE
################################################################################

# LOAD THE FUNCTION FOR COMPUTING THE MINIMUM MORTALITY DISTRIBUTION
source("findmin.R")

# SET SEED TO CREATE REPRODUCIBLE RESULTS
set.seed(1010)

# FIND THE MINIMUM MORTALITY DISTRIBUTION
tempdist <- findmin(cb,model,at=CTmortality$temp,sim=TRUE,nsim=5000)

# DEFINE THE CENTRAL ESTIMATE AND 95% CONFIDENCE INTERVAL
ci <- quantile(tempdist,c(0.025,0.5,0.975))

# SET THE REFERENCE TEMPERATURE AS THE UPPER RANGE OF THE MORTALITY MINIMUM
mintemp <- ci[3]

# DEFINE THE EXTREME HEAT CUTOFF TEMPERATURE
exheattemp <- quantile(CTmortality$temp,exheatperc/100,na.rm=T)
exexheattemp <- quantile(CTmortality$temp,exexheatperc/100,na.rm=T)

rm(tempdist, ci)

#