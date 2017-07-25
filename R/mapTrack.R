mapTrack <- function(input = rel_rec,what.species = 'SKJ',what.longitude='longitude',what.latitude='latitude',nbins=100)
{
  #input = rel_rec; what.species = c("SKJ")
  #world <- map_data('world');
  #ggplot() + geom_polygon(data=world,aes(x=long,y=lat,group=group),col='darkgreen',fill='darkgreen') +
    
    
    Atl <- c(-30,0)
  input <- fortify(input)
  input <- input[input$score>8,]
  wAfMap <- get_map(location=Atl,source='google',maptype='satellite',crop=TRUE,zoom=3)
  ggmap(wAfMap) +
    coord_fixed(1,xlim=c(-70,25),ylim=c(-40,50))  +
   geom_segment(data=input[input$speciescode %in% what.species,],
                 aes(x=longitude,y=latitude,xend=rec_longitude,yend=rec_latitude,group=specimenid,color=speciescode),
              arrow = arrow(length = unit(0.2,"cm")),size=.3,alpha=.25) +
  facet_wrap(~ speciescode, ncol=2) +
    theme(plot.margin=unit(c(1,1,1,1),"cm"),
          axis.text.x =element_text(colour="grey20",size=12,face="plain"),
          axis.text.y=element_text(colour="grey20",size=12,face="plain"),
          axis.title.y=element_text(size=15))+
    xlab("")+ylab("")
  
  
}
