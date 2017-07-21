spatialVectors <- function(rel_rec=rel_rec)
{
  
data("eez") # add on names of EEZs
data("seas")
data("lme") # large marine ecosystems
data("fadmoratorium")

# make sure projections are all the same

proj4string(lme) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(eez) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(fadmoratorium) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(seas) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# add on vectors for release locations

# find missing release locations

  xx <- (1:length(rel_rec[,1]))[is.na(rel_rec$longitude)]
  yy <- (1:length(rel_rec[,1]))[is.na(rel_rec$latitude)]
  xx.yy<-unique(c(xx,yy))
rel_rec_na <- rel_rec[xx.yy,]
rel_rec1 <- rel_rec[-c(xx.yy),]

  rel_rec_xy <- data.frame(longitude=rel_rec1$longitude,latitude=rel_rec1$latitude)
  coordinates(rel_rec_xy) <- c("longitude", "latitude")
  proj4string(rel_rec_xy) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  rel_rec_xy <- SpatialPointsDataFrame(rel_rec_xy,data=data.frame(speciescode=rel_rec1$speciescode))
  
  # Add on EEZ
  
  rel_rec1$eez <- as.character(over(rel_rec_xy, eez)$EEZ)
  rel_rec1$eez[is.na(rel_rec1$eez)] <- 'High Seas'
  rel_rec1$eez[rel_rec1$eez == "Western Saharan Exclusive Economic Zone"] <- "Mauritanian Exclusive Economic Zone"
  rel_rec1$eez[rel_rec1$eez == "Disputed Western Sahara/Mauritania"]      <- "Mauritanian Exclusive Economic Zone"
  rel_rec1$eez <- gsub('Exclusive Economic Zone','EEZ',rel_rec1$eez)
  
  # Add on LME
  rel_rec1$lme <- as.character(over(rel_rec_xy, lme)$LME_NAME)
  # Add on Ocean or Sea
  rel_rec1$ocean <- as.character(over(rel_rec_xy, seas)$NAME)
  
  rel_rec_na$eez <- NA
  rel_rec_na$lme <- NA
  rel_rec_na$ocean <- NA
  
  rel_rec <- rbind(rel_rec1,rel_rec_na)
  
  #########################################
  # add on vectors for release locations###
  #########################################
  
  # find missing recovery locations
  
  xx <- (1:length(rel_rec[,1]))[is.na(rel_rec$rec_longitude)]
  yy <- (1:length(rel_rec[,1]))[is.na(rel_rec$rec_latitude)]
  xx.yy<-unique(c(xx,yy))
  rel_rec_na <- rel_rec[xx.yy,]
  rel_rec1 <- rel_rec[-c(xx.yy),]
  
  rel_rec_xy <- data.frame(longitude=rel_rec1$rec_longitude,latitude=rel_rec1$rec_latitude)
  coordinates(rel_rec_xy) <- c("longitude", "latitude")
  proj4string(rel_rec_xy) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  rel_rec_xy <- SpatialPointsDataFrame(rel_rec_xy,data=data.frame(speciescode=rel_rec1$speciescode))
  
  # Add on EEZ
  
  rel_rec1$rec_eez <- as.character(over(rel_rec_xy, eez)$EEZ)
  rel_rec1$rec_eez[is.na(rel_rec1$eez)] <- 'High Seas'
  rel_rec1$rec_eez[rel_rec1$eez == "Western Saharan Exclusive Economic Zone"] <- "Mauritanian Exclusive Economic Zone"
  rel_rec1$rec_eez[rel_rec1$eez == "Disputed Western Sahara/Mauritania"]      <- "Mauritanian Exclusive Economic Zone"
  rel_rec1$rec_eez <- gsub('Exclusive Economic Zone','EEZ',rel_rec1$eez)
  
  # Add on LME
  rel_rec1$rec_lme <- as.character(over(rel_rec_xy, lme)$LME_NAME)
  # Add on Ocean or Sea
  rel_rec1$rec_ocean <- as.character(over(rel_rec_xy, seas)$NAME)
  
  rel_rec_na$rec_eez <- NA
  rel_rec_na$rec_lme <- NA
  rel_rec_na$rec_ocean <- NA
  
  rel_rec <- rbind(rel_rec1,rel_rec_na)
  
  
  rel_rec
  
  
  
  
}