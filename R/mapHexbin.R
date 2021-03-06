mapHexbin <- function(input = rel_rec,what.species = 'SKJ',
                      what.longitude='longitude',
                      what.latitude='latitude',nbins=100)
  {
   # input = rel_rec; what.longitude = "rclonx"; what.latitude="rclaty"; 
   # what.species = c("YFT","BET","SKJ");nbins=120
  world <- map_data('world');
  my_breaks <- round(exp(seq(log(1), log(800), length = 5)))
    ggplot(data=input[input$speciescode %in% what.species,],aes_string(x=what.longitude,y=what.latitude))+
      coord_fixed(1.3,xlim=c(-90,30),ylim=c(-50,50))+
      #facet_wrap(~speciescode,ncol=2)+
      #geom_point(aes(x=longitude,y=latitude),data=input[input$speciescode %in% what.species,],size=1,color='green')+
      geom_hex(bins = nbins )+ 
      scale_fill_gradientn(colours=heat.colors(10),trans='log',labels=my_breaks,breaks=my_breaks)+
      #geom_polygon(data=eez,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=0.4)+
      geom_polygon(data=world,aes(x=long,y=lat,group=group),col='darkgreen',fill='darkgreen')+
    theme(plot.margin=unit(c(0,0,0,0),"cm"),strip.text = element_text(size=15),
          axis.text.x =element_text(colour="grey20",size=12,face="plain"),
          axis.text.y=element_text(colour="grey20",size=12,face="plain"),
          axis.title.y=element_text(size=12))+
      xlab("")+ylab("")
    
  
}

