relRecSummaryTab <- function(input=rel_rec)
{

xyt1 <- table(input$speciescode,input$rcstagecode)
Total_Rel_Col <- apply(xyt1,1,sum)

Releases <-cbind(xyt1,Total_Rel_Col)
Total_Rel_Row <- apply(Releases,2,sum)
Releases <- rbind(Releases,Total_Rel_Row)
#pander(Releases)

Total_Rec <- table(input$recovered,input$speciescode)[2,]
Perc_Rec <- round((Total_Rec/Total_Rel_Col),2)*100
Recoveries <- rbind(Total_Rel_Col,Total_Rec,Perc_Rec)
trel <- sum(Recoveries[1,])
trec <- sum(Recoveries[2,])
avP  <- round((trec/trel),1)*100
Recoveries <- cbind(Recoveries,c(trel,trec,avP))
#pander(Recoveries)
out<-list(Releases=Releases,Recoveries=Recoveries)
out
}