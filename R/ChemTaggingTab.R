ChemTaggingTab <- function(input=rel_rec){
  chem_tagged <- input[input$ctcolor1==2,]
  ct <- table(chem_tagged$speciescode,chem_tagged$recovered)
  ct[,1]<-ct[,1]+ct[,2]
  dimnames(ct)[[2]]<- c('released','recovered')
  pp <- round((ct[,"recovered"]/ct[,"released"]*100),1)
  Chem_Tagged <- rbind(Chem_Tagged_Rel=ct[,"released"],Chem_Tagged_Rec=ct[,"recovered"],Chem_Tagged_Perc=pp)
  Chem_Tagged
}


