nTagsRelByCountry <- function(input=rel_rec){
  
  input$taggerid[input$taggerid ==991] <-992
  input$personname <- as.character(persons$personname[match(input$taggerid,persons$personid)])
  persons$country  <-   as.character(countries$country[match(persons$personcountryid,countries$countryid)])
  input$personcountryid <- as.character(persons$country[match(input$taggerid,persons$personid)])
  input$personcountryid[is.na(input$personcountryid)] <- 'Unknown'
  #xx<- input[input$personcountryid == 'Unknown',]
  # table(xx$taggerid)
  # 
  # 0   925   926   927  3099 10000 # these taggerids are not in the persons database
  # 1658   422   713   842     1     7 
  
  fbyc <- melt(table(input$personcountryid))
  fbyc<-rbind(fbyc,c(NA,NA))
  fbyc$value[dim(fbyc)[1]] <- sum(fbyc$value,na.rm=T)
  fbyc$Var1 <- as.character(fbyc$Var1)
  fbyc$Var1[dim(fbyc)[1]] <- "Total"
  colnames(fbyc)<-c("Country","Frequency")
  pander(fbyc)
  
}