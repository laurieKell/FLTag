
## Script to download data from Aottp tagging datbase and prepare for inclusion in FLTag ## 

#1. Install RODBC

library(RODBC)

#2. Connect to database

aottp <- odbcConnect("aottp-servigis", case="postgresql", believeNRows=FALSE)

#3. Get releases 

releases <- sqlQuery(aottp, "SELECT * from releases;")

#4. Prune the data

releases <- releases[,c(2,4,5,6:18,20,21,22,23,24,25,26,27,28,29,47,52)]


#5. Get recoveries

recoveries <- sqlQuery(aottp, "SELECT * from recoveries WHERE rcstagecode LIKE 'RCF';")

#6. Prune the recoveries

recoveries<- recoveries[,c(2,5:19,22,26:28,44)]

#5. Save the data 

save (releases,file='/home/dbeare/FLTag/data/releases.RData',compress="xz")
save (recoveries,file='/home/dbeare/FLTag/data/recoveries.RData',compress="xz")











