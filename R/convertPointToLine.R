convertPointToLine <- function(input = rel_rec)
{
  input = rel_rec
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
