relRecSummaryTab <- function(input=rel_rec)
{
#input <- rel_rec
#rci <- which(rel_rec$rcstagecode_final %in% c("R-2","R-3","R-4","R-5","RCF"))        # Where are the recoveries
#rei <- which(rel_rec$rcstagecode_final %in% c("R-1","R-2","R-3","R-4","R-5","RCF"))  # Where are the releases

tab_Rels_Recs_tots <- table(input$speciescode,input$recovered)
dimnames(tab_Rels_Recs_tots)[[2]]<-c('Nos_Released','Nos_Recovered')
Totals_Col <- apply(tab_Rels_Recs_tots,2,sum)
tab_Rels_Recs_tots <- rbind(tab_Rels_Recs_tots,Totals_Col)

tab_Rels_Recs_tots[,'Nos_Released'] <- tab_Rels_Recs_tots[,'Nos_Recovered']+tab_Rels_Recs_tots[,'Nos_Released']


Perc_Rec <- round((tab_Rels_Recs_tots[,'Nos_Recovered']/tab_Rels_Recs_tots[,'Nos_Released'])*100,1)

tab_Rels_Recs_tots <- cbind(tab_Rels_Recs_tots,Perc_Rec)

tab_Rels_Recs_tots <- as.data.frame(tab_Rels_Recs_tots)

tab_Rels_Recs_tots

}