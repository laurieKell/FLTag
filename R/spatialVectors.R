spatialVectors <- function(input=rel_rec)
{
#input <- rel_rec
#input<- iotc[1:10000,]
data("eez") # add on names of EEZs
data("seas")
#data("lme") # large marine ecosystems
data("fadmoratorium")

# make sure projections are all the same

#proj4string(lme) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(eez) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(fadmoratorium) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(seas) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# add on vectors for release locations

# find missing release locations

  xx <- (1:length(input[,1]))[is.na(input$relonx)]
  yy <- (1:length(input[,1]))[is.na(input$relaty)]
  xx.yy<-unique(c(xx,yy))

 if(length(xx.yy) > 0)
{
input_na <- input[xx.yy,]
input1 <- input[-c(xx.yy),]
#print('Missing locations')
}
else
  {
#print('No missing locations')
input1 <- input
}

  input_xy <- data.frame(longitude=input1$relonx,latitude=input1$relaty)
  coordinates(input_xy) <- c("longitude", "latitude")
  proj4string(input_xy) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  
  input_xy <- SpatialPointsDataFrame(input_xy,data=data.frame(speciescode=input1$speciescode))
  
  # Add on EEZ
  
  input1$reeez <- as.character(over(input_xy, eez)$EEZ)
  input1$reeez[is.na(input1$eez)] <- 'High Seas'
  input1$reeez[input1$eez == "Western Saharan Exclusive Economic Zone"] <- "Mauritanian Exclusive Economic Zone"
  input1$reeez[input1$eez == "Disputed Western Sahara/Mauritania"]      <- "Mauritanian Exclusive Economic Zone"
  input1$reeez <- gsub('Exclusive Economic Zone','EEZ',input1$reeez)
  # Add on LME
  #input1$relme <- as.character(over(input_xy, lme)$LME_NAME)
  # Add on Ocean or Sea
  input1$reocean <- as.character(over(input_xy, seas)$NAME)
  
  #In FadMoratorium-releases
  
  input1$refmor <- over(input_xy, fadmoratorium)$re

  
  
  if(length(xx.yy) > 0)
    {
  input_na$reeez <- NA
  #input_na$relme <- NA
  input_na$reocean <- NA
  input_na$refmor  <-NA
  input <- rbind(input1,input_na)
  }
  
  else{
    input <- input1
  }
  
  input$requad <- rep(NA,length(input[,1]))
  input$requad[input$relonx >= -30 & input$relaty >= 10] <- 'NE'
  input$requad[input$relonx >= -30 & input$relaty <= 10] <- 'SE'
  
  input$requad[input$relonx < -30 & input$relaty > 10] <- 'NW'
  input$requad[input$relonx < -30 & input$relaty < 10] <- 'SW'
  

  #########################################
  # add on vectors for recovery locations###
  #########################################
  
  # find missing recovery locations
  
  xx <- (1:length(input[,1]))[is.na(input$rclonx)]
  yy <- (1:length(input[,1]))[is.na(input$rclaty)]
  xx.yy<-unique(c(xx,yy))
  input_na <- input[xx.yy,]
  input1 <- input[-c(xx.yy),]
  
  input_xy <- data.frame(longitude=input1$rclonx,latitude=input1$rclaty)
  coordinates(input_xy) <- c("longitude", "latitude")
  proj4string(input_xy) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  input_xy <- SpatialPointsDataFrame(input_xy,data=data.frame(speciescode=input1$speciescode))
  
  # Add on EEZ
  
  input1$rceez <- as.character(over(input_xy, eez)$EEZ)
  input1$rceez[is.na(input1$rceez)] <- 'High Seas'
  input1$rceez[input1$rceez == "Western Saharan Exclusive Economic Zone"] <- "Mauritanian Exclusive Economic Zone"
  input1$rceez[input1$rceez == "Disputed Western Sahara/Mauritania"]      <- "Mauritanian Exclusive Economic Zone"
  input1$rceez <- gsub('Exclusive Economic Zone','EEZ',input1$rceez)
  
  # Add on LME
  #input1$rclme <- as.character(over(input_xy, lme)$LME_NAME)
  # Add on Ocean or Sea
  input1$rcocean <- as.character(over(input_xy, seas)$NAME)
  
  #In FadMoratorium-recoveries
  
  input1$rcfmor <- over(input_xy, fadmoratorium)$re
  
  
  input_na$rceez <- NA
  #input_na$rclme <- NA
  input_na$rcocean <- NA
  input_na$rcfmor <- NA
  
  input <- rbind(input1,input_na)
  
  input$rcquad <- rep(NA,length(input[,1]))
  input$rcquad[input$rclonx >= -30 & input$rclaty >= 10] <- 'NE'
  input$rcquad[input$rclonx >= -30 & input$rclaty <= 10] <- 'SE'
  
  input$rcquad[input$rclonx < -30 & input$rclaty > 10] <- 'NW'
  input$rcquad[input$rclonx < -30 & input$rclaty < 10] <- 'SW'
  
  
 
  
  
  input
  
}



