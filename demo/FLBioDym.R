# FLBioDym - A package for fitting biomass dynamic models
# FLBioDym/demo/FLBioDym.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurence Kell, ICCAT
# $Id: FLBioDym.R 9 2009-01-26 14:58:25Z ltkell $

library(FLBioDym)

# load up example data set from
# Polacheck,T., Hilborn,R., and Punt.A.E, Fitting Surplus Production Models: Comparing Methods and Measuring Uncertainty.
# Can. J. Fish. Aquat. Sci. 50(12): 2597–2607 (1993)
data(albSP)

# Summarise and inspect the object
summary(albSP)

# fit
albSP<-fit(albSP,start=c(r=0.3,K=mean(catch(albSP))*10))
plot(   albSP)

# look at goodness of fit
plot(albSP,type="diag")


# look at surplus production curve
plot(albSP,type="equil")

# calculate reference points
refpts(albSP)
msy( albSP)
bmsy(albSP)
fmsy(albSP)

# parameters
params(albSP)

# t tests
albSP@stats

# covariance estimates
vcov(albSP)

# estimated parameters
# look at likilihood profile
r.  <-seq(0.1, .6, length.out=100)
logl<-tapply(r.,1:100,function(x) {fit(albSP,fixed=c(r=x),start=c(r=.3, K=mean(catch(albSP))*10))@logLik})
plot(logl~r.,type="l")
points(c(albSP@params["r",]),albSP@logLik,pch=16,col="red",cex=2)

# Uncertainty in reference points
refptSE(albSP)

# Jacknife
albJK<-albSP
index(albJK)<-jacknife(index(albJK))
albJK<-fit(albJK,start=c(r=0.3,K=mean(catch(albJK))*10))

plot(albJK)

