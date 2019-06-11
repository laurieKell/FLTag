mapPoints <- function (input = rel_rec,what.species = c('BET','SKJ','YFT'),what.longitude='relonx',what.latitude='relaty',
                       lon.limits=c(-80,30),lat.limits=c(-50,50),what.size=1)
  
{
  # input = rel_rec; what.longitude = "relonx"; what.latitude="relaty"; what.species = c("BET","YFT","SKJ");
  # lon.limits<-c(-80,30);what.size <- 2
  # lat.limits<-c(-50,50)
  input <- fortify(input)
  #wAfMap <- get_map(location=location,source='osm',maptype='satellite',crop=TRUE,zoom=zz)
  world <- map_data('world');
  
  ggplot(data=input[input$speciescode %in% what.species,],aes_string(x=what.longitude,y=what.latitude))+
    coord_fixed(1.3,xlim=lon.limits,ylim=lat.limits)+
    #facet_wrap(~speciescode,ncol=2)+
    
    geom_point(data=input[input$speciescode %in% what.species,],aes_string(x=what.longitude,y=what.latitude,color="speciescode"),
               alpha=1,size=what.size) +
    #geom_polygon(data=eez,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=0.4)+
    geom_polygon(data=world,aes(x=long,y=lat,group=group),col='darkgreen',fill='darkgreen')+
    
    geom_hline(yintercept=10, color='green',size=.2)+
    geom_vline(xintercept=-30, color='green',size=.2)+
    
    #geom_polygon(data=fadmoratorium,aes(x=long,y=lat,group=group),color='orange',size=0.2) +
    #geom_polygon(data=lme,aes(x=long,y=lat,group=group),color='darkgreen',fill=NA,size=0.5) +
    
    #geom_polygon(data=eez,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=0.1) +
    
    theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"),
          axis.text.x =element_text(colour="grey20",size=12,face="plain"),
          axis.text.y=element_text(colour="grey20",size=12,face="plain"),
          axis.title.y=element_text(size=15))+
    xlab("")+ylab("")
  
}
