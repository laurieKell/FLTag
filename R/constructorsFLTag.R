# constructors - constructor methods for FLTag
# FLTag/R/constructors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell

setGeneric('FLTag', function(object, ...)
		standardGeneric('FLTag'))

setMethod('FLTag', signature(object='FLQuant'),
  function(object,model="pellat",...)
    {
    args <- list(...)

    # empty object
    object[]<- NA
    dims    <- dims(object)
    res     <- new("FLTag")

    catch(res)<-object
    index(res)<-object
    range(res)<-unlist(list(minyear=dims$minyear, maxyear=dims$maxyear))

    res@model <-model
    res@params<-setParams(model)

    # Load given slots
  	for(i in names(args))
			slot(res, i) <- args[[i]]

    return(res)
    }
)

setMethod('FLTag', signature(object='missing'),
  function(...)
    {
    args <- list(...)

    # if no FLQuant argument given, then use empty FLQuant
    slots <- lapply(args, class)
    slots <- names(slots)[slots == 'FLQuant']
    if(length(slots) == 0)
      object <- FLQuant()
    else
      object <- args[[slots[1]]]

    return(FLTag(object, ...))
    }
)

is.FLTag <- function(x)
	return(inherits(x, "FLTag"))
