# FLTag -
# FLR4SCRS/R/FLTag.R

# Reference:
# Notes:

setGeneric("I",              function(object,...)
	standardGeneric("I"))
setGeneric('O',              function(object, ...)
	standardGeneric("O"))
setGeneric('computeTag.n',   function(object, ...)
	standardGeneric("computeTag.n"))
setGeneric('computeTag.p',   function(object, ...)
	standardGeneric("computeTag.p"))
setGeneric('computeTag.r',   function(object, ...)
	standardGeneric("computeTag.r"))
setGeneric('computeStock.n', function(object, ...)
	standardGeneric("computeStock.n"))

setMethod('I', signature(object='FLQuant'),
  function(object,...){
    dmns          <-dimnames(object)
    dmns[[1]]     <-ac((dims(object)$minyear-dims(object)$max):(dims(object)$maxyear- dims(object)$min))
    names(dmns)[1]<-"quant"
    flc           <-FLQuant(NA,dimnames=dmns)

    t.            <-as.data.frame(object)
    t.$cohort     <-t.$year-t.$age
    flc[]         <-daply(t.,c("cohort","year","unit","season","area","iter"),function(x) sum(x$data))

    return(flc)})

setMethod('O', signature(object='FLQuant'),
  function(object,...){
    dmns          <-dimnames(object)
    dmns[[1]]     <-ac((dims(object)$maxyear-dims(object)$max):(dims(object)$minyear-dims(object)$min))
    names(dmns)[1]<-"age"
    flc           <-FLQuant(NA,dimnames=dmns)

    t.            <-as.data.frame(object)
    t.$age        <-t.$year-t.$quant
    t.            <-t.[!is.na(t.$data),]
    flc[]         <-daply(t.,c("age","year","unit","season","area","iter"),function(x) sum(x$data))

    return(flc)})

setMethod('computeTag.p', signature(object='FLTag'),
  function(object,...)
   tagP*exp(-harvest-m)[-1,-1])

setMethod('computeTag.n', signature(object='FLTag'),
  function(object,...)
   harvest/(harvest+m)*pTag*(1-exp(-harvest-m)))

setMethod('computeTag.r', signature(object='FLTag'),
  function(object,...)
   harvest/(harvest+m)*stock.n*(1-exp(-harvest-m)))

setMethod('computeStock.n', signature(object='FLTag'),
  function(object,...)
   stock.n*exp(-harvest-m)[-1,-1])


#loglAR1(m(ple4), m(ple4))

