
## Script to populate FLTag with tagging data and shapefiles (EEZ, Large Marine Ecosystems) ## 

#1. Install RODBC

library(RODBC)

#2. Connect to database

aottp <- odbcConnect("aottp-servigis", case="postgresql", believeNRows=FALSE)

#3. Get releases 

releases <- sqlQuery(aottp, "SELECT * from releases;")

#4. Prune the data

releases <- releases[,c(2,4,9,12,15,16,17,18,21,25,26,52)]

#5. Get recoveries

recoveries <- sqlQuery(aottp, "SELECT * from recoveries WHERE rcstagecode LIKE 'RCF';")

#6. Prune the recoveries

recoveries<- recoveries[,c(2,5,10,13,16,17,18,19,22,26)]

#7. Save the data 

save (releases,file='/home/dbeare/FLTag/data/releases.RData',compress="xz")
save (recoveries,file='/home/dbeare/FLTag/data/recoveries.RData',compress="xz")


## Shapefiles ##

library(rgdal)

#1. Read in Large Marine Ecosystems 

lme <- readOGR("/media/aottp/AOTTP/DataExploration/GISData", "LME66")

proj4string(lme) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#2. Read in EEZs 

eez <- readOGR("/media/aottp/AOTTP/DataExploration/GISData","World_EEZ_v8_2014_HR")

proj4string(eez) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#3. Write out data for FLTag library

save (eez,file='/home/dbeare/FLTag/data/eez.RData',compress="xz")
save (lme,file='/home/dbeare/FLTag/data/lme.RData',compress="xz")




