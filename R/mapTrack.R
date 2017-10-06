mapTrack <- function(input = rel_rec,what.species = 'SKJ',what.gear = c('PS','BB','GILL','UNKN','LL','SPOR'),what.size=1)
{
  #input = rel_rec; what.species = c("SKJ");what.gear=c('BB')
  #world <- map_data('world');
  #ggplot() + geom_polygon(data=world,aes(x=long,y=lat,group=group),col='darkgreen',fill='darkgreen') +
    
    
    Atl <- c(-30,0)
    input <- input[input$speciescode %in% what.species,]
    input <- input[input$rec_gearcode %in% what.gear,]
  input <- fortify(input)
  wAfMap <- get_map(location=Atl,source='google',maptype='satellite',crop=TRUE,zoom=3)
  ggmap(wAfMap) +
    coord_fixed(1,xlim=c(-70,25),ylim=c(-40,50))  +
   geom_segment(data=input,
                 aes(x=longitude,y=latitude,xend=rec_longitude,yend=rec_latitude,group=specimenid,color=speciescode),
              arrow = arrow(length = unit(0.4,"cm")),size=what.size,alpha=1) +
  facet_wrap(~ speciescode, ncol=2) +
    theme(plot.margin=unit(c(1,1,1,1),"cm"),
          axis.text.x =element_text(colour="grey20",size=12,face="plain"),
          axis.text.y=element_text(colour="grey20",size=12,face="plain"),
          axis.title.y=element_text(size=15))+
    xlab("")+ylab("")
  
  
}













