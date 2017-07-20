
## Script to populate FLTag with tagging data and shapefiles (EEZ, Large Marine Ecosystems) ## 

#1. Install RODBC

library(RODBC)

#2. Connect to database

aottp <- odbcConnect("aottp-local", case="postgresql", believeNRows=FALSE)

#3. Get IOTC data

iotc <- read.table('/home/dbeare/Dropbox/AOTTP/Data/IOTC/IOTC_tagging.csv',header=T, sep="|",skip=2)
sqlSave(aottp,iotc,table='iotc')

iotc <- sqlQuery(aottp,"SELECT * FROM iotc;")

iotc <- iotc[,-1]

save (iotc,file='/home/dbeare/FLTag/data/iotc.RData',compress="gzip")


#4. Get AOTTP releases and recoveries

releases <- sqlQuery(aottp, "SELECT * from releases;")
recoveries <- sqlQuery(aottp, "SELECT * from recoveries WHERE rcstagecode LIKE 'RCF';")

#5. Prune the data

releases <- data.frame(speciescode = releases$speciescode,rcstagecode=releases$rcstagecode,ctcode1=releases$ctcode1,ctcode2=releases$ctcode2,latitude=releases$latitude,
                       longitude=releases$longitude,gearcode=releases$gearcode,depth=releases$depth,len=releases$len,specimenid=releases$specimenid, date=releases$date,time=releases$time)

## Format the columns properly
releases$speciescode <- as.character(releases$speciescode)
releases$rcstagecode <- as.character(releases$rcstagecode)
releases$release_date <- strptime(paste(releases$date,releases$time), format = "%Y-%m-%d %H:%M:%S") 
releases <- releases[,-c(11,12)]
releases$gearcode <- as.character(releases$gearcode)

#6. Get recoveries

recoveries <- sqlQuery(aottp, "SELECT * from recoveries WHERE rcstagecode LIKE 'RCF';")

#7. Prune the recoveries

recoveries<- data.frame(speciescode = recoveries$speciescode,rcstagecode=recoveries$rcstagecode,ctcode1=recoveries$ctcode1,ctcode2=recoveries$ctcode2,latitude=recoveries$latitude,
                        longitude=recoveries$longitude,gearcode=recoveries$gearcode,len=recoveries$len,specimenid=recoveries$specimenid, date=recoveries$date,time=recoveries$time)


recoveries$rcstagecode <- as.character(recoveries$rcstagecode)
recoveries$recovery_date <- strptime(paste(recoveries$date,recoveries$time), format = "%Y-%m-%d %H:%M:%S") 
recoveries <- recoveries[,-c(10,11)]
recoveries$gearcode <- as.character(recoveries$gearcode)

#7. Save the data 

save (releases,file='/home/dbeare/FLTag/data/releases.RData',compress="gzip")
save (recoveries,file='/home/dbeare/FLTag/data/recoveries.RData',compress="gzip")

#8. Get historical ICCAT releases and recoveries 

releases.past <- sqlQuery(aottp, "SELECT * from re_iccat_all;")

releases.past <- data.frame(speciescode = releases.past$speciescode,rcstagecode=releases.past$rcstagecode,ctcode1=releases.past$ctcode1,ctcode2=releases.past$ctcode2,latitude=releases.past$latitude,
                       longitude=releases.past$longitude,gearcode=releases.past$gearcode,depth=releases.past$depth,len=releases.past$len,specimenid=releases.past$specimenid, date=releases.past$date,time=releases.past$time)

## Format the columns properly
releases.past$speciescode <- as.character(releases.past$speciescode)
releases.past$rcstagecode <- as.character(releases.past$rcstagecode)
releases.past$release_date <- strptime(paste(releases.past$date,releases.past$time), format = "%Y-%m-%d %H:%M:%S") 
releases.past <- releases.past[,-c(11,12)]
releases.past$gearcode <- as.character(releases.past$gearcode)


recoveries.past <- sqlQuery(aottp, "SELECT * from rc_iccat_all;")

recoveries<- data.frame(speciescode = recoveries$speciescode,rcstagecode=recoveries$rcstagecode,ctcode1=recoveries$ctcode1,ctcode2=recoveries$ctcode2,latitude=recoveries$latitude,
                        longitude=recoveries$longitude,gearcode=recoveries$gearcode,len=recoveries$len,specimenid=recoveries$specimenid, date=recoveries$date,time=recoveries$time)


recoveries$rcstagecode <- as.character(recoveries$rcstagecode)
recoveries$recovery_date <- strptime(paste(recoveries$date,recoveries$time), format = "%Y-%m-%d %H:%M:%S") 
recoveries <- recoveries[,-c(10,11)]
recoveries$gearcode <- as.character(recoveries$gearcode)








## Format the columns properly


















## Shapefiles ##

library(rgdal)

#1. Read in Large Marine Ecosystems 

lme <- readOGR("/media/aottp/AOTTP/DataExploration/GISData", "LME66")

proj4string(lme) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#2. Read in EEZs 

eez <- readOGR("/media/aottp/AOTTP/DataExploration/GISData","World_EEZ_v8_2014_HR")

proj4string(eez) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

library(rgeos)

eez.small <- gSimplify(eez,tol=1,topologyPreserve=TRUE)

#3. Write out data for FLTag library

save (eez.small,file='/home/dbeare/FLTag/data/eezsmall.RData',compress="xz")
save (lme,file='/home/dbeare/FLTag/data/lme.RData',compress="xz")

#4. FAD moratorium area 2016

l1 <- cbind(c(-20,-20), c(-4,5))
l2 <- cbind(c(-20,-9.05), c(5,5))
l3 <- cbind(c(-20,11.38),c(-4,-4))

Sl1 <- Line(l1)
Sl2 <- Line(l2)
Sl3 <- Line(l3)

S1 <- Lines(list(Sl1), ID = "a")
S2 <- Lines(list(Sl2), ID = "b")
S3 <- Lines(list(Sl3), ID = "c")

Sl <- SpatialLines(list(S1, S2, S3))


library(rgeos)
df <- data.frame(len = sapply(1:length(Sl), function(i) gLength(Sl[i, ])))
rownames(df) <- sapply(1:length(Sl), function(i) Sl@lines[[i]]@ID)

## SpatialLines to SpatialLinesDataFrame
library(rgdal)

Sldf <- SpatialLinesDataFrame(Sl, data = df)
proj4string(Sldf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
system('rm -r /home/dbeare/fadmoratorium')
writeOGR(Sldf,"/home/dbeare/fadmoratorium",layer="fadmoratorium" , driver="ESRI Shapefile")

fadmoratorium <- readOGR("/home/dbeare/fadmoratorium",layer="fadmoratorium")

save(fadmoratorium,file='/home/dbeare/FLTag/data/fadmoratorium.RData',compress="xz")














