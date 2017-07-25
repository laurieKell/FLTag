ChemTaggingTab <- function(input=rel_rec){
  chem_tagged <- input[input$ctcolor1 == 2,]
  ct <- table(chem_tagged$speciescode)
  rec_chem_tagged <- input[!is.na(input$rec_ctcolor1) & input$rec_ctcolor1 == 2,]
  ct1 <- table(rec_chem_tagged$speciescode)
  Chem_Tagged <- rbind(Chem_Tagged_Rel=ct,Chem_Tagged_Rec=ct1,Chem_Tagged_Perc=round(ct1/ct,3)*100)
  Chem_Tagged
}

