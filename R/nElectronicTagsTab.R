nElectronicTagsTab <- function(input=rel_rec,what.species=c('BET','SKJ','YFT')){
  #input<-rel_rec
  ninput <- input[!is.na(input$electronictagcode1) & input$speciescode %in% what.species,]
  ninput <- ninput[ninput$rcstagecode == 'R-1',]
  eTagRel <- table(ninput$speciescode,ninput$model)
  eTagRec <- table(ninput$speciescode[ninput$recovered==T],ninput$model[ninput$recovered==T])
  eTagPerc <- (eTagRec/eTagRel)*100
  out<-list(eTagRel=eTagRel,eTagRec=eTagRec,eTagPerc=round(eTagPerc,1))
  out
  }