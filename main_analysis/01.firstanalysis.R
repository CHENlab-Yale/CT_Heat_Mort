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
# RUN THE MODEL
################################################################################

# DEFINE THE CROSSBASIS
argvar <- list(fun=varfun,df=dfarg)
cb <- crossbasis(CTmortality$temp,lag=lag,argvar=argvar,
                 arglag=list(fun=lagfun))

# RUN THE MODEL AND OBTAIN PREDICTIONS
# CENTERING NOT NEEDED AS IT DOES NOT AFFECT COEF/VCOV
model <- glm(formula,family=quasipoisson,CTmortality,na.action="na.exclude")

#