
## Script to download data from Aottp tagging datbase and prepare for inclusion in FLTag ## 

#1. Install RODBC

library(RODBC)

#2. Connect to database

aottp <- odbcConnect("aottp-servigis", case="postgresql", believeNRows=FALSE)

#3. Get releases 

releases <- sqlQuery(aottp, "SELECT * from releases;")

#4. Get recoveries

recoveries <- sqlQuery(aottp, "SELECT * from recoveries;")

#5. Save the data 

save (releases,file='/home/dbeare/FLTag/tests/releases.RData',compress=T)
save (recoveries,file='/home/dbeare/FLTag/tests/recoveries.RData',compress=T)

