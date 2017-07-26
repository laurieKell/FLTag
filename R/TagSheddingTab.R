
TagSheddingTab <- function(input=rel_rec)
  
{
  #input <- rel_rec
  #Basic numbers double tagged
  dt <- ifelse(is.na(input$ctcode2),"Single_Totals","Double_Totals")
  tdt <-table(dt,input$speciescode)
  tdt <- cbind(tdt,apply(tdt,1,sum))
  Double_Tag_Nos <-rbind(tdt,Double_Tag_Perc=round(tdt[1,]/tdt[2,],2)*100)
  
  # Tag-Shedding
  d1 <- input[!is.na(input$ctcode2),] # all releases that were double-tagged
  input1 <- d1[d1$recovered == TRUE,]
  Total_Double_Tagged <- table(input1$speciescode)
  
  t1 <- ifelse(is.na(input1$rec_ctcode1),1,0)
  t2 <- ifelse(is.na(input1$rec_ctcode2),1,0)
  
  Lost_Left <- table(t1,input1$speciescode)
  Lost_Right <- table(t2,input1$speciescode)
  Tag_Shed_Nos <- rbind(Lost_Left=Lost_Left[2,],Lost_Right=Lost_Right[2,],Total_Double_Tagged)
  Tag_Shed_Perc <- round(Tag_Shed_Nos[1:2,]/rbind(Tag_Shed_Nos[3,],Tag_Shed_Nos[3,]),3)*100
  
  out<- list(Double_Tag_Nos=Double_Tag_Nos,Tag_Shed_Nos=Tag_Shed_Nos,Tag_Shed_Perc=Tag_Shed_Perc)
  out
}
