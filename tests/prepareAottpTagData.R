
## Script to populate FLTag with tagging data and shapefiles (EEZ, Large Marine Ecosystems) ## 

#1. Install RODBC

library(RODBC)

#2. Connect to database

aottp <- odbcConnect("aottp-local", case="postgresql", believeNRows=FALSE)

#3. Get IOTC data

# iotc <- read.table('/home/dbeare/Dropbox/AOTTP/Data/IOTC/IOTC_tagging.csv',header=T, sep="|",skip=2)
# sqlSave(aottp,iotc,table='iotc')
# iotc <- sqlQuery(aottp,"SELECT * FROM iotc;")
# iotc <- iotc[,-1]
#save (iotc,file='/home/dbeare/FLTag/data/iotc.RData',compress="gzip")

#4. Get AOTTP releases and recoveries

releases <- sqlQuery(aottp, "SELECT * from releases WHERE chktagcanceled ='false';") # Get ALL the data

#5. Prune the data

releases <- data.frame(speciescode = releases$speciescode,rcstagecode=releases$rcstagecode,ctcode1=releases$ctcode1,ctcode2=releases$ctcode2,latitude=releases$latitude,
                       longitude=releases$longitude,gearcode=releases$gearcode,depth=releases$depth,len=releases$len,specimenid=releases$specimenid, date=releases$date,time=releases$time)

## Format the columns properly
releases$speciescode <- as.character(releases$speciescode)
releases$rcstagecode <- as.character(releases$rcstagecode)
releases$date <- strptime(releases$date, format = "%Y-%m-%d")
releases$time <- as.character(releases$time) 
releases$gearcode <- as.character(releases$gearcode)

#6. Recoveries

recoveries <- sqlQuery(aottp, "SELECT * from recoveries WHERE rcstagecode LIKE 'RCF';")

#7. Prune the recoveries

recoveries<- data.frame(speciescode = recoveries$speciescode,rcstagecode=recoveries$rcstagecode,ctcode1=recoveries$ctcode1,ctcode2=recoveries$ctcode2,latitude=recoveries$latitude,
                        longitude=recoveries$longitude,gearcode=recoveries$gearcode,depth=rep(NA,length(recoveries[,1])),len=recoveries$len,specimenid=recoveries$specimenid, date=recoveries$date,time=recoveries$time)

recoveries$rcstagecode <- as.character(recoveries$rcstagecode)
recoveries$date <- strptime(recoveries$date, format = "%Y-%m-%d") 
recoveries$time <- as.character(recoveries$time)
recoveries$gearcode <- as.character(recoveries$gearcode)

##################################################
#8. Get historical ICCAT releases and recoveries##
##################################################

# ## releases
# releases.past <- sqlQuery(aottp, "SELECT * from re_iccat_all;")
# releases.past <- data.frame(speciescode = releases.past$speciescode,rcstagecode=releases.past$rcstage,ctcode1=releases.past$ctcode1,ctcode2=releases.past$ctcode2,latitude=releases.past$latitude,
#                        longitude=releases.past$longitude,gearcode=releases.past$gearcode,depth=releases.past$depth,len=releases.past$len,specimenid=releases.past$specimenid, date=releases.past$date,time=releases.past$time)
# 
# ## Format the columns properly
# releases.past$speciescode <- as.character(releases.past$speciescode)
# releases.past$rcstagecode <- as.character(releases.past$rcstagecode)
# releases.past$date <- strptime(releases.past$date, format = "%Y-%m-%d") 
# releases.past$time <-as.character(releases.past$time)
# releases.past$gearcode <- as.character(releases.past$gearcode)
# 
# # Recoveries
# recoveries.past <- sqlQuery(aottp, "SELECT * from rc_iccat_all;")
# 
# recoveries.past<- data.frame(speciescode = recoveries.past$speciescode,rcstagecode=recoveries.past$rcstagecode,ctcode1=recoveries.past$ctcode1,ctcode2=recoveries.past$ctcode2,latitude=recoveries.past$latitude,
#                         longitude=recoveries.past$longitude,depth=rep(NA,length(recoveries.past[,1])),gearcode=recoveries.past$gearcode,len=recoveries.past$len,specimenid=recoveries.past$specimenid, 
#                         date=recoveries.past$date,time=recoveries.past$time)
# 
# 
# recoveries.past$rcstagecode <- as.character(recoveries.past$rcstagecode)
# recoveries.past$date <- strptime(recoveries.past$date, format = "%Y-%m-%d") 
# recoveries.past$time <- as.character(recoveries.past$time)
# recoveries.past$gearcode <- as.character(recoveries.past$gearcode)


# Combine the old with the new

# releases <- rbind(releases,releases.past)
# recoveries <- rbind(recoveries,recoveries.past)

releases$depth <- ifelse(releases$depth==0,NA,releases$depth)

# Look at the stagecodes ?

# 9. Save the data 

save (releases,file='/home/dbeare/FLTag/data/releases.RData',compress="gzip")
save (recoveries,file='/home/dbeare/FLTag/data/recoveries.RData',compress="gzip")

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

#5. Shapefile of seas/oceans

seas <- readOGR("/media/aottp/AOTTP/DataExploration/GISData/seas/World_Seas.shp", "World_Seas")

proj4string(seas) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

save(seas,file='/home/dbeare/FLTag/data/seas.RData',compress="xz")

# 
# As part of the validation process data are placed in the following three categories:
# 
# 1. Fully documented and validated (both release and recoveries with precise geographic locations, correct species identifications and sensible lengths)
# 
# 2. Validated movement (both release and recoveries with precise geographic locations and correct species identifications)
# 
# 3. Validated growth (both release and recoveries with exact dates and reliable lengths)








