nElectronicTagsTab <- function(input=etags,what.species=c('BET','SKJ','YFT')){
  #input<-etags;what.species <- c('BET','SKJ','YFT')
  ninput <- input[input$speciescode %in% what.species,]
  #ninput <- ninput[!is.na(ninput$electronictagcode1),]
  #ninput <- ninput[ninput$rcstagecode == 'R-1',] # some of the tags have been re-used.
  rci <- which(ninput$rc_rcstagecode %in% c("R-2","R-3","R-4","R-5","RCF"))        # Where are the recoveries
  ninput$recovered <- FALSE
  ninput$recovered[rci] <- TRUE
  #ninput <- ninput[ninput$rc_rcstagecode == 'R-1',]
  
  tt <- table(ninput$speciescode,ninput$supplier,ninput$recovered)
  eTagRel <- tt[,,'FALSE']+tt[,,'TRUE']
  eTagRec <- tt[,,'TRUE']
  
  eTagPerc <- (eTagRec/eTagRel)*100
  out<-list(eTagRel=eTagRel,eTagRec=eTagRec,eTagPerc=round(eTagPerc,1))
  out
  }