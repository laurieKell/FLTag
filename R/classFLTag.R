# class - «Short one line description»
# FLTag/R/class.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell
# Last Change: 7 July 2009 16:17

validFLTag <- function(object)
  {
  ## Catch must be continous
  yrs<-dimnames(stock.n(object))$year
  
  if (!all(yrs == ac(dims(stock.n(object))$minyear:dims(stock.n(object))$maxyear)))
      return("years in catch not continous")

  # range
  dims <-dims(object)
  range<-as.list(object@range)

  return(TRUE)}

setClass('FLTag',
  representation(
    "FLStock",
    survival      ='FLQuant',
    tag.p         ='FLQuant',
    tag.n         ='FLQuant',
    recov.r       ='FLQuant',
    index         ='FLQuant',
    model         ="character",
    distribution  ="character",
    params        ='data.frame',
    vcov          ='array',
    hessian       ='array',
    logLik        ='numeric',
    rsdlVar       ='numeric',
    dof           ='array',
    stats         ='array',
    stopmess      ="character"),
  prototype(
    range         =unlist(list(minyear=as.numeric(NA), maxyear=as.numeric(NA))),
    survival      =FLQuant(),
    tag.p         =FLQuant(),
    tag.n         =FLQuant(),
    recov.r       =FLQuant(),
    index         =FLQuant(),
    model         ="brownie",
    distribution  ="log"),
	validity=validFLTag)
