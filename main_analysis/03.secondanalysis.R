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
# OBTAIN PREDICTIONS FROM THE FITTED MODEL CENTERED AT THE REFERENCE TEMPERATURE
################################################################################

# CREATE THE MATRIX TO STORE THE RELATIVE RISKS
rr <- matrix(NA,nrow=99,ncol=5,dimnames=list(c(1:99),c("percentile","temp","RR","RRlow","RRhigh")))

# DEFINE THE QUANTILES
predvar <- quantile(CTmortality$temp,1:99/100,na.rm=T)

# RUN THE CROSSPRED FUNCTION CENTERED AT THE REFERENCE TEMPERATURE
pred <- crosspred(cb,model,at=c(predvar),cen=mintemp,cumul=TRUE)

# DEFINE THE COEFFICIENT AND VARIANCE-COVARIANCE
coef <- coef(pred)
vcov <- vcov(pred)

# DEFINE RELATIVE RISK FOR A SAME-DAY LAG
rr[,"percentile"] <- c(1:99)
rr[,"temp"] <- pred$predvar
rr[,"RR"] <- pred$matRRfit[,"lag0"]
rr[,"RRlow"] <- pred$matRRlow[,"lag0"]
rr[,"RRhigh"] <- pred$matRRhigh[,"lag0"]
rr <- as.data.frame(rr)

#