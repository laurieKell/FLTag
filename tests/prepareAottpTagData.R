
## Script to populate FLTag with tagging data and shapefiles (EEZ, Large Marine Ecosystems) ## 

#1. Install RODBC

library(RODBC)

#2. Connect to database

aottp <- odbcConnect("aottp-servigis", case="postgresql", believeNRows=FALSE)

#3. Get releases 

releases <- sqlQuery(aottp, "SELECT * from releases;")

#4. Prune the data

releases <- releases[,c(2,4,9,12,15,16,17,18,21,25,26,52)]

# Format the columns properly
releases$speciescode <- as.character(releases$speciescode)
releases$rcstagecode <- as.character(releases$rcstagecode)
releases$release_date <- strptime(paste(releases$date,releases$time), format = "%Y-%m-%d %H:%M:%S") 
releases <- releases[,-c(5,6)]
releases$gearcode <- as.character(releases$gearcode)
dimnames(releases)[[2]] <- c("speciescode","rcstagecode","ctcode1","ctcode2","latitude","longitude","gearcode","depth","len",         
"specimenid","date")

#5. Get recoveries

recoveries <- sqlQuery(aottp, "SELECT * from recoveries WHERE rcstagecode LIKE 'RCF';")

#6. Prune the recoveries

recoveries<- recoveries[,c(2,5,10,13,16,17,18,19,22,26)]


# Format the columns properly

recoveries$rcstagecode <- as.character(recoveries$rcstagecode)
recoveries$recovery_date <- strptime(paste(recoveries$date,recoveries$time), format = "%Y-%m-%d %H:%M:%S") 
recoveries <- recoveries[,-c(5,6)]
recoveries$gearcode <- as.character(recoveries$gearcode)
dimnames(recoveries)[[2]] <- c("specimenid","rcstagecode","ctcode1","ctcode2","latitude","longitude","gearcode",
                               "len","date")

#7. Save the data 

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














