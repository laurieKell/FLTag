# coerce - cpoercion methods for FLTag
# FLTag/R/coerce.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell
# Last Change: 12 Mar 2009 14:56

setAs('FLTag', 'FLIndex',
	function(from)
	  {
    res  <-do.call(new, c(list(Class='FLTag'),args))

    range(res)<-range(from,c("minyear","maxyear"))
    name( res)<-name( from)
    desc( res)<-paste("created from FLIndex", desc(from))

    catch(res)<-apply(catch.n(from)*catch.wt(from),c(2,6),sum)
    if (type=="biomass")
       index(res)[[1]]<-apply(index(from),c(2,6),sum)

    return(res)
    })

setAs('FLTag', 'FLStock',
	function(from)
	  {
    res  <-do.call(new, c(list(Class='FLTag'),args))

    range(res)<-range(from,c("minyear","maxyear"))
    name( res)<-name( from)
    desc( res)<-paste("created from FLStock", desc(from))

    catch(res)<-computeCatch.n(from)

    return(res)
    })
