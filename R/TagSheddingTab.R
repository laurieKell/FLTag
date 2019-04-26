TagSheddingTab <- function(input=rel_rec)
  
{
  #input <- rel_rec
  #Basic numbers double tagged
  dt <- ifelse(is.na(input$numtag2),"Single_Totals","Double_Totals")
  tdt <-table(dt,input$speciescode)
  tdt <- cbind(tdt,apply(tdt,1,sum))
  Double_Tag_Nos <-rbind(tdt,Double_Tag_Perc=round(tdt[1,]/tdt[2,],2)*100)
  
  # Tag-Shedding
  d1 <- input[!is.na(input$numtag2),] # all releases that were double-tagged
  input1 <- d1[d1$recovered == TRUE,] # all releases that were recovered
  Total_Double_Tagged <- table(input1$speciescode) # total number of releases that were recovered
  
  
  d2 <- input[!is.na(input$numtag1),] # all releases that were tagged
  input2 <- d2[d2$recovered == TRUE,] # all releases that were recovered
  Total_Tagged <- table(input2$speciescode) # total number of releases that were recovered
  
  
  #t1 <- ifelse(is.na(input1$rec_ctcode1),1,0)
  t2 <- ifelse(is.na(input1$rcnumtag2),1,0)
  
  #Lost_Left <-  table(t1,input1$speciescode)
  Lost_Right <- table(t2,input1$speciescode)
  
  Tag_Shed_Nos <- rbind(Lost=Lost_Right[2,],Total_Double_Tagged)
  Tag_Shed_Perc <- round(Tag_Shed_Nos["Lost",]/Tag_Shed_Nos["Total_Double_Tagged",],3)*100
  out<- list(Double_Tag_Nos=Double_Tag_Nos,Tag_Shed_Nos=Tag_Shed_Nos,Tag_Shed_Perc=Tag_Shed_Perc)
  out
  
  # Tag_Shed_Nos <- rbind(Lost_Left=Lost_Left[2,],Lost_Right=Lost_Right[2,],Total_Double_Tagged)
  # Tag_Shed_Perc <- round(Tag_Shed_Nos[1:2,]/rbind(Tag_Shed_Nos[3,],Tag_Shed_Nos[3,]),3)*100
  # 
  # out<- list(Double_Tag_Nos=Double_Tag_Nos,Tag_Shed_Nos=Tag_Shed_Nos,Tag_Shed_Perc=Tag_Shed_Perc)
  # out

}







