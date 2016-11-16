
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
dimnames(releases)[[2]] <- c("rel_speciescode","rcstagecode","ctcode1","ctcode2","rel_latitude","rel_longitude","rel_gearcode","rel_depth","rel_len",         
"specimenid","rel_date")

#5. Get recoveries

recoveries <- sqlQuery(aottp, "SELECT * from recoveries WHERE rcstagecode LIKE 'RCF';")

#6. Prune the recoveries

recoveries<- recoveries[,c(2,5,10,13,16,17,18,19,22,26)]


# Format the columns properly

recoveries$rcstagecode <- as.character(recoveries$rcstagecode)
recoveries$recovery_date <- strptime(paste(recoveries$date,recoveries$time), format = "%Y-%m-%d %H:%M:%S") 
recoveries <- recoveries[,-c(5,6)]
recoveries$gearcode <- as.character(recoveries$gearcode)
dimnames(recoveries)[[2]] <- c("specimenid","rcstagecode","ctcode1","ctcode2","rec_latitude","rec_longitude","rec_gearcode",
                               "rel_len","rec_date")

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

library(rgeos)

eez.small <- gSimplify(eez,tol=1,topologyPreserve=TRUE)

#3. Write out data for FLTag library

save (eez.small,file='/home/dbeare/FLTag/data/eezsmall.RData',compress="xz")
save (lme,file='/home/dbeare/FLTag/data/lme.RData',compress="xz")




