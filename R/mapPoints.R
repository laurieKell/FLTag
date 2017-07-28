mapPoints <- function (input = rel_rec,what.species = 'SKJ',what.longitude='longitude',what.latitude='latitude')
  
{
  #input = rel_rec; what.longitude = "longitude"; what.latitude="latitude"; what.species = c("YFT","BET")
  
  Atl <- c(-30,0)
  input <- fortify(input)
  wAfMap <- get_map(location=Atl,source='google',maptype='satellite',crop=TRUE,zoom=3)
  ggmap(wAfMap) +
    #coord_fixed(1.3,xlim=c(-40,30),ylim=c(-40,45)) +
    geom_point(data=input[input$speciescode %in% what.species,],aes_string(x=what.longitude,y=what.latitude,color="speciescode"),alpha=0.5,size=.5) +
    #geom_polygon(data=fadmoratorium,aes(x=long,y=lat,group=group),color='orange',size=0.2) +
    #geom_polygon(data=lme,aes(x=long,y=lat,group=group),color='darkgreen',fill=NA,size=0.5) +
    #geom_polygon(data=eez,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=0.1) +
    facet_wrap(~ speciescode, ncol=2)+
    theme(plot.margin=unit(c(1,1,1,1),"cm"),
          axis.text.x =element_text(colour="grey20",size=12,face="plain"),
          axis.text.y=element_text(colour="grey20",size=12,face="plain"),
          axis.title.y=element_text(size=15))+
    xlab("")+ylab("")
  
}
