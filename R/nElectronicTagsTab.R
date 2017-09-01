nElectronicTagsTab <- function(input=rel_rec,what.species=c('BET','SKJ','YFT')){
  #input<-rel_rec;what.species <- c('BET','SKJ','YFT')
  ninput <- input[input$speciescode %in% what.species,]
  ninput <- ninput[!is.na(ninput$electronictagcode1) & ninput$rcstagecode == 'R-1',]
  #ninput <- ninput[ninput$rcstagecode == 'R-1',] # some of the tags have been re-used.
  tt <- table(ninput$speciescode,ninput$model,ninput$recovered)
  eTagRel <- tt[,,'FALSE']
  eTagRec <- tt[,,'TRUE']
  
  eTagPerc <- (eTagRec/eTagRel)*100
  out<-list(eTagRel=eTagRel,eTagRec=eTagRec,eTagPerc=round(eTagPerc,1))
  out
  }