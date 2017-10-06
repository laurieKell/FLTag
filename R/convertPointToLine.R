convertPointToLine <- function(input = rel_rec)
{
  #input = rel_rec
  #world <- map_data('world');
  #ggplot() + geom_polygon(data=world,aes(x=long,y=lat,group=group),col='darkgreen',fill='darkgreen') +
  

test <- input[!is.na(input$longitude) & !is.na(input$latitude) & !is.na(input$rec_latitude) & input$recovered ==T,]
test1 <- data.frame(longitude=test$longitude,latitude=test$latitude,rec_long=test$rec_longitude,rec_lat=test$rec_latitude,
                    speciescode=test$speciescode,electronictagcode1=test$electronictagcode1,timestamp=test$timestamp,rec_timestamp=test$rec_timestamp,rec_gearcode=test$rec_gearcode)

V = lapply(1:nrow(test), 
           function(i){return(list(L=Line(matrix(unlist(test1[i,][1:4]),ncol=2,byrow=TRUE)),i=i))})


VV = SpatialLines(lapply(V, function(E){Lines(list(E$L),as.character(E$i))}))
plot(VV)


SLDF = SpatialLinesDataFrame(VV,data=test1)

SLDF

}

#plot(SLDF[SLDF@data$rec_gearcode == 'PS' & SLDF@data$speciescode == 'SKJ',])
#plot(SLDF[SLDF@data$rec_gearcode == 'BB' & SLDF@data$speciescode == 'BET',])

# library(raster)
# 
# skj.ras <- SLDF[SLDF@data$rec_gearcode == 'PS' & SLDF@data$speciescode == 'SKJ',]
# 
# r <- raster(ncol=100, nrow=100)
# extent(r) <- extent(skj.ras)
# 
# skj.ras@data$timestamp <- as.POSIXct(skj.ras@data$timestamp)
# skj.ras@data$rec_timestamp <- as.POSIXct(skj.ras@data$rec_timestamp)
# skj.ras@data$weekn <- week(skj.ras@data$timestamp)
# skj.ras@data$dayn <- floor(julian(skj.ras@data$timestamp,origin="2016-01-01"))
# 
# wo <- sort(unique(skj.ras@data$weekn))
# 
# for(i in wo )
#   
# { print(i)
#   Demo_ras = rasterize(skj.ras[skj.ras@data$weekn == i,],r)
#   par(mfrow=c(1,1))
#   image(Demo_ras,xlab="",ylab="",xlim=c(-50,50),ylim=c(-50,50))
#   map('world',add=T,fill=T,col='green')
# }
# 
# slotNames(Demo_ras)
# x<-Demo_ras@data@values
# summary(x)
# atbs <- Demo_ras@data@attributes
# atbs[[1]][1:50,]
# 
# points(releases$longitude[releases$speciescode == 'SKJ'], releases$latitude[releases$speciescode == 'SKJ'],pch=16)














