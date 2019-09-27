mapPointsSpeciesByMonth <- function (input = rel_rec,what.species = 'SKJ',what.longitude='relonx',what.latitude='relaty',
                                     what.facet='reyrmon',ncol=4,lon.limits=c(-80,30),lat.limits=c(-50,50),what.size=.2)
  
{
 
  #input = rel_rec; what.longitude = "rec_longitude"; what.latitude="rec_latitude"; what.species = c('BET','SKJ','YFT');ncol=4
  #input = tt17; what.longitude = "longitude"; what.latitude="latitude"; what.species = c('BET','LTA','SKJ','YFT');ncol=4;
  #what.facet='yrmon'
  
  #input <- input[input$tagseeding == 0,] # Don't plot tag seeding
  
  Atl <- c(-30,0)
  input <- fortify(input[input$speciescode %in% what.species,])
  #wAfMap <- get_map(location=Atl,source='google',maptype='satellite',crop=TRUE,zoom=3)
  world <- map_data('world');
  ggplot(data=input[input$speciescode %in% what.species,],
    aes_string(x=what.longitude,y=what.latitude))+
    coord_fixed(1.3,xlim=lon.limits,ylim=lat.limits) +
    geom_point(data=input,aes_string(x=what.longitude,y=what.latitude,
                                     color="speciescode"),alpha=1,size=what.size)+
    geom_polygon(data=world,aes(x=long,y=lat,group=group),col='darkgreen',fill='darkgreen')+
  geom_hline(yintercept=10, color='green',size=.05)   +
    geom_vline(xintercept=-30, color='green',size=.05)   +
    #geom_polygon(data=fadmoratorium,aes(x=long,y=lat,group=group),color='orange',size=0.2) +
    #geom_polygon(data=lme,aes(x=long,y=lat,group=group),color='darkgreen',fill=NA,size=0.5) +
    #geom_polygon(data=eez,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=0.1) +
    facet_wrap(what.facet, ncol=ncol)+
    theme(legend.position="right")+
    theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"),
          axis.text.x =element_text(colour="grey20",size=5,face="plain"),
          axis.text.y=element_text(colour="grey20",size=5,face="plain"),
          axis.title.y=element_text(size=8))+
    xlab("")+ylab("")
  
}

