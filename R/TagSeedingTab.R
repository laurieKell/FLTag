TagSeedingTab <- function(input=tseed){

  tagSeedDat <- input
#Releases first

rci <- which(tagSeedDat$rcstagecode %in% c("R-2","R-3","R-4","R-5","RCF"))        # Where are the recoveries
tagSeedDat$recovered <- FALSE
tagSeedDat$recovered[rci] <- TRUE


tagSeed <- data.frame(table(tagSeedDat$speciescode,tagSeedDat$tsplace,tagSeedDat$recovered))
colnames(tagSeed)<-c('speciescode','tsplace','recovered','Freq')
ntsp <- length(unique(tagSeed$tsplace))
tagSeed <- dcast(tagSeed,recovered+speciescode~tsplace,value.var='Freq')
tagSeedRec <-   tagSeed[tagSeed$recovered==TRUE,]
tagSeedF <-     tagSeed[tagSeed$recovered==FALSE,]
tagSeedRel <- tagSeedRec
tagSeedRel[,c(3:(ntsp+2))] <- tagSeedF[,c(3:(ntsp+2))]+tagSeedRec[,c(3:(ntsp+2))]
tagSeedPerc <- tagSeedRel
tagSeedPerc[,c(3:(ntsp+2))] <- tagSeedRec[,c(3:(ntsp+2))]/tagSeedRel[,c(3:(ntsp+2))]
tagSeedPerc[,c(3:(ntsp+2))] <- round(tagSeedPerc[,c(3:(ntsp+2))],3)*100

out<- list(tagSeedRel=tagSeedRel[,-1],tagSeedRec=tagSeedRec[,-1],tagSeedPerc=tagSeedPerc[,-1])

out

}