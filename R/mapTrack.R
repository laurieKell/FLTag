
mapTrack <- function(input = rel_rec,what.species = 'SKJ',what.gear = c('PS','BB','GILL','UNKN','LL','SPOR'),
                     what.size=1,lon.limits=c(-80,50),lat.limits=c(-50,50),what.distance = 3)
{
  # input <-  elec[!is.na(elec$days_at_liberty) & elec$days_at_liberty > 365 & elec$supplier == 'LOTEK LAT2810',]
   #what.species = c("BET","YFT");  
 #what.gear = c('PS','BB','GILL','UNKN','LL','SPOR');what.size<-1;
  # what.distance <- 3;lon.limits=c(-80,50);lat.limits=c(-50,50)
   # input <- ltk
    input <- input[input$speciescode %in% what.species,]
    input <- input[input$rcgearcode %in% what.gear,]
    input <- input[!is.na(input$relonx),]
    input <- input[!is.na(input$relaty),]
    input <- input[input$distance_nm >= what.distance,]
    input <- input[!is.na(input$distance_nm),]
    #input$quarter <- quarter(input$redate)
    #input$remonth = factor(input$remonth, levels = month.name)
  input <- fortify(input)
  #wAfMap <- get_map(location=location,source='google',maptype='satellite',crop=TRUE,zoom=zz)
  #ggmap(wAfMap) +
    world <- map_data('world');
    ggplot()+
    geom_polygon(data=world,aes(x=long,y=lat,group=group),col='black',fill='darkgreen')+
  
    coord_fixed(1,xlim=lon.limits,ylim=lat.limits)  +
   geom_segment(data=input,
                 aes(x=relonx,y=relaty,xend=rclonx,yend=rclaty,group=specimenid,
                   color=speciescode),
              arrow = arrow(length = unit(0.1,"cm")),size=what.size,alpha=1) +
      geom_point(data=input,
                 aes(x=relonx,y=relaty),
                 alpha=1,size=.5,color=5) +
      #facet_wrap(~ reyrmon, ncol=4) +
      #facet_wrap( ~ requarter, ncol=2) +
      #scale_color_gradient(low="red", high="yellow") +
    theme(plot.margin=unit(c(1,1,1,1),"cm"),
          axis.text.x =element_text(colour="grey20",size=10,face="plain"),
          axis.text.y=element_text(colour="grey20",size=10,face="plain"),
          axis.title.y=element_text(size=12))+
    xlab("")+ylab("")
}


# 
# mapTrack(input=rel_rec, what.species ='YFT',what.size=.2, what.distance = 500)









