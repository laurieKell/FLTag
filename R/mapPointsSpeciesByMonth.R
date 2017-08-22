mapPointsSpeciesByMonth <- function (input = rel_rec,what.species = 'SKJ',what.longitude='longitude',what.latitude='latitude',ncol=4)
  
{
 # input = rel_rec; what.longitude = "longitude"; what.latitude="latitude"; what.species = 'SKJ'
  
  input <- input[input$tagseeding == 0,] # Don't plot tag seeding
  
  Atl <- c(-30,0)
  input <- fortify(input[input$speciescode == what.species,])
  wAfMap <- get_map(location=Atl,source='google',maptype='satellite',crop=TRUE,zoom=3)
  ggmap(wAfMap) +
    #coord_fixed(1.3,xlim=c(-40,30),ylim=c(-40,45)) +
    geom_point(data=input,aes_string(x=what.longitude,y=what.latitude,color="speciescode"),alpha=0.5,size=.5) +
    geom_hline(yintercept=10, color='green',size=.2)+
    geom_vline(xintercept=-30, color='green',size=.2)+
    
    #geom_polygon(data=fadmoratorium,aes(x=long,y=lat,group=group),color='orange',size=0.2) +
    #geom_polygon(data=lme,aes(x=long,y=lat,group=group),color='darkgreen',fill=NA,size=0.5) +
    #geom_polygon(data=eez,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=0.1) +
    facet_wrap(~ yrmon, ncol=ncol)+
    theme(legend.position="none")+
    theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"),
          axis.text.x =element_text(colour="grey20",size=10,face="plain"),
          axis.text.y=element_text(colour="grey20",size=10,face="plain"),
          axis.title.y=element_text(size=11))+
    xlab("")+ylab("")
  
}

