spatialVectors <- function(input=rel_rec)
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

  xx <- (1:length(input[,1]))[is.na(input$longitude)]
  yy <- (1:length(input[,1]))[is.na(input$latitude)]
  xx.yy<-unique(c(xx,yy))
input_na <- input[xx.yy,]
input1 <- input[-c(xx.yy),]

  input_xy <- data.frame(longitude=input1$longitude,latitude=input1$latitude)
  coordinates(input_xy) <- c("longitude", "latitude")
  proj4string(input_xy) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  input_xy <- SpatialPointsDataFrame(input_xy,data=data.frame(speciescode=input1$speciescode))
  
  # Add on EEZ
  
  input1$eez <- as.character(over(input_xy, eez)$EEZ)
  input1$eez[is.na(input1$eez)] <- 'High Seas'
  input1$eez[input1$eez == "Western Saharan Exclusive Economic Zone"] <- "Mauritanian Exclusive Economic Zone"
  input1$eez[input1$eez == "Disputed Western Sahara/Mauritania"]      <- "Mauritanian Exclusive Economic Zone"
  input1$eez <- gsub('Exclusive Economic Zone','EEZ',input1$eez)
  
  # Add on LME
  input1$lme <- as.character(over(input_xy, lme)$LME_NAME)
  # Add on Ocean or Sea
  input1$ocean <- as.character(over(input_xy, seas)$NAME)
  
  input_na$eez <- NA
  input_na$lme <- NA
  input_na$ocean <- NA
  input <- rbind(input1,input_na)
  
  input$quad <- rep(NA,length(input[,1]))
  input$quad[input$longitude >= -30 & input$latitude >= 10] <- 'NE'
  input$quad[input$longitude >= -30 & input$latitude <= 10] <- 'SE'
  
  input$quad[input$longitude < -30 & input$latitude > 10] <- 'NW'
  input$quad[input$longitude < -30 & input$latitude < 10] <- 'SW'
  
  
  #########################################
  # add on vectors for recovery locations###
  #########################################
  
  # find missing recovery locations
  
  xx <- (1:length(input[,1]))[is.na(input$rec_longitude)]
  yy <- (1:length(input[,1]))[is.na(input$rec_latitude)]
  xx.yy<-unique(c(xx,yy))
  input_na <- input[xx.yy,]
  input1 <- input[-c(xx.yy),]
  
  input_xy <- data.frame(longitude=input1$rec_longitude,latitude=input1$rec_latitude)
  coordinates(input_xy) <- c("longitude", "latitude")
  proj4string(input_xy) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  input_xy <- SpatialPointsDataFrame(input_xy,data=data.frame(speciescode=input1$speciescode))
  
  # Add on EEZ
  
  input1$rec_eez <- as.character(over(input_xy, eez)$EEZ)
  input1$rec_eez[is.na(input1$rec_eez)] <- 'High Seas'
  input1$rec_eez[input1$rec_eez == "Western Saharan Exclusive Economic Zone"] <- "Mauritanian Exclusive Economic Zone"
  input1$rec_eez[input1$rec_eez == "Disputed Western Sahara/Mauritania"]      <- "Mauritanian Exclusive Economic Zone"
  input1$rec_eez <- gsub('Exclusive Economic Zone','EEZ',input1$rec_eez)
  
  # Add on LME
  input1$rec_lme <- as.character(over(input_xy, lme)$LME_NAME)
  # Add on Ocean or Sea
  input1$rec_ocean <- as.character(over(input_xy, seas)$NAME)
  
  input_na$rec_eez <- NA
  input_na$rec_lme <- NA
  input_na$rec_ocean <- NA
  
  input <- rbind(input1,input_na)
  
  input$rec_quad <- rep(NA,length(input[,1]))
  input$rec_quad[input$rec_longitude >= -30 & input$rec_latitude >= 10] <- 'NE'
  input$rec_quad[input$rec_longitude >= -30 & input$rec_latitude <= 10] <- 'SE'
  
  input$rec_quad[input$rec_longitude < -30 & input$rec_latitude > 10] <- 'NW'
  input$rec_quad[input$rec_longitude < -30 & input$rec_latitude < 10] <- 'SW'
  
  input
  
}