mapTrack <- function(input = rel_rec,what.species = 'SKJ',what.gear = c('PS','BB','GILL','UNKN','LL','SPOR'),
                     what.size=1,lon.limits=c(-80,30),lat.limits=c(-50,50))
{
  #input = rel_rec; what.species = c("YFT");  
  #what.gear = c('PS','BB','GILL','UNKN','LL','SPOR');
  
  #what.size<-1
    input <- input[input$speciescode %in% what.species,]
    input <- input[input$regearcode %in% what.gear,]
  input <- fortify(input)
  #wAfMap <- get_map(location=location,source='google',maptype='satellite',crop=TRUE,zoom=zz)
  #ggmap(wAfMap) +
    world <- map_data('world');
    ggplot()+
    geom_polygon(data=world,aes(x=long,y=lat,group=group),col='black',fill='darkgreen')+
  
    coord_fixed(1,xlim=lon.limits,ylim=lat.limits)  +
   geom_segment(data=input,
                 aes(x=relonx,y=relaty,xend=rclonx,yend=rclaty,group=specimenid,color=speciescode),
              arrow = arrow(length = unit(0.2,"cm")),size=what.size,alpha=1) +
  facet_wrap(~ reyrmon, ncol=5) +
    theme(plot.margin=unit(c(1,1,1,1),"cm"),
          axis.text.x =element_text(colour="grey20",size=12,face="plain"),
          axis.text.y=element_text(colour="grey20",size=12,face="plain"),
          axis.title.y=element_text(size=15))+
    xlab("")+ylab("")
}













