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
# COMPUTE THE ATTRIBUTABLE DEATHS WITH EMPIRICAL CONFIDENCE INTERVAL
################################################################################

# LOAD THE FUNCTION FOR COMPUTING THE ATTRIBUTABLE RISK MEASURES
source("attrdl.R")

# CREATE THE MATRIX TO STORE THE ATTRIBUTABLE DEATHS
simlist <- c()

# NUMBER OF SIMULATION RUNS FOR COMPUTING EMPIRICAL CI
nsim <- 10000

# CREATE THE MATRIX TO STORE THE CI OF ATTRIBUTABLE DEATHS
matsim <- array(NA,dim=c(5,nsim),dimnames=list(c("tot","cold","heat","exheat","exexheat")))

################################################################################
# SET SEED TO CREATE REPRODUCIBLE RESULTS
set.seed(1010)

# COMPUTE THE ATTRIBUTABLE DEATHS
simlist[1] <- attrdl(CTmortality$temp,cb,CTmortality$Total,model,
                         type="an",dir="forw",cen=mintemp)
simlist[2] <- attrdl(CTmortality$temp,cb,CTmortality$Total,model,
                          type="an",dir="forw",cen=mintemp,range=c(-100,mintemp))
simlist[3] <- attrdl(CTmortality$temp,cb,CTmortality$Total,model,
                          type="an",dir="forw",cen=mintemp,range=c(mintemp,100))
simlist[4] <- attrdl(CTmortality$temp,cb,CTmortality$Total,model,
                            type="an",dir="forw",cen=mintemp,range=c(exheattemp,100))
simlist[5] <- attrdl(CTmortality$temp,cb,CTmortality$Total,model,
                     type="an",dir="forw",cen=mintemp,range=c(exexheattemp,100))

# COMPUTE EMPIRICAL OCCURRENCES OF THE ATTRIBUTABLE DEATHS
# USED TO DERIVE CONFIDENCE INTERVALS
matsim["tot",] <- attrdl(CTmortality$temp,cb,CTmortality$Total,model,
                            type="an",dir="forw",cen=mintemp,sim=T,nsim=nsim)
matsim["cold",] <- attrdl(CTmortality$temp,cb,CTmortality$Total,model,
                             type="an",dir="forw",cen=mintemp,
                             range=c(-100,mintemp),sim=T,nsim=nsim)
matsim["heat",] <- attrdl(CTmortality$temp,cb,CTmortality$Total,model,
                             type="an",dir="forw",cen=mintemp,
                             range=c(mintemp,100),sim=T,nsim=nsim)
matsim["exheat",] <- attrdl(CTmortality$temp,cb,CTmortality$Total,model,
                               type="an",dir="forw",cen=mintemp,
                               range=c(exheattemp,100),sim=T,nsim=nsim)
matsim["exexheat",] <- attrdl(CTmortality$temp,cb,CTmortality$Total,model,
                            type="an",dir="forw",cen=mintemp,
                            range=c(exexheattemp,100),sim=T,nsim=nsim)

# STORE THE DENOMINATOR OF ATTRIBUTABLE DEATHS, I.E. TOTAL OBSERVED MORTALITY
totdeath <- sum(CTmortality$Total,na.rm=T)

################################################################################
# CREATE THE MATRIX TO STORE THE ATTRIBUTABLE DEATHS
an <- matrix(NA,3,5,dimnames=list(c("an","low","high"),c("tot","cold","heat","exheat","exexheat")))
af <- matrix(NA,3,5,dimnames=list(c("af","low","high"),c("tot","cold","heat","exheat","exexheat")))

# ATTRIBUTABLE NUMBERS
an["an",] <- simlist
an["low",] <- apply(matsim,1,quantile,0.025)
an["high",] <- apply(matsim,1,quantile,0.975)

# ATTRIBUTABLE FRACTIONS
af["af",] <- an[1,]/totdeath*100
af["low",] <- an[2,]/totdeath*100
af["high",] <- an[3,]/totdeath*100

rm(simlist, matsim, nsim)
#