mapScatterpie <- function (input = rel_rec,what.longitude='longitude',what.latitude='latitude',what.species = c('BET','LTA','SKJ','YFT'),what.yrmon='yrmon',sf=2)
 
{
  
 
  
  #input <- rel_rec; what.longitude = "longitude"; what.latitude="latitude"; what.species = c('BET','SKJ','YFT');what.yrmon='yrmon';sf=4
  input <- input[!is.na(input$score),]
  ns <- length(what.species)
  
  
  fish_rel <- data.frame(longitude=input[,what.longitude],latitude=input[,what.latitude],yrmon=input[,what.yrmon],speciescode=input$speciescode)
  fish_rel0 <- table(round(fish_rel$longitude,1),round(fish_rel$latitude,1),fish_rel$yrmon,fish_rel$speciescode)
  fish_rel1 <- melt(fish_rel0)
  dimnames(fish_rel1)[[2]] <- c('longitude','latitude','yrmon','speciescode','freq')
  fish_rel1 <- fish_rel1[fish_rel1$freq>0,]
  fish_rel2 <- fish_rel1[fish_rel1$speciescode %in% what.species,]
  fish_rel3 <- reshape(fish_rel2,timevar="speciescode",idvar=c("longitude","latitude","yrmon"),direction = "wide")
  for(i in seq(4,ns+(ns-1))){
  fish_rel3[,i] <- ifelse(is.na(fish_rel3[,i]),0,fish_rel3[,i])}
  fish_rel3$region <- 1:length(fish_rel3[,1])
  fish_rel3$radius <- apply(fish_rel3[,seq(4,ns+(ns-1))],1,sum,na.rm=T)
  #for(i in seq(4,length(what.species)+3)){
  #  print(i);fish_rel3[,i] <- fish_rel3[,i]/fish_rel3$radius}
  #fish_rel3$radius <- 1
  fish_rel3$radius <- log(fish_rel3$radius)
  #fish_rel3$radius <- fish-rel3$radius/sf
  
  print(head(fish_rel3))
  world <- map_data('world')
  ggplot(world, aes(long, lat, group=group)) +
  geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
    geom_hline(yintercept=10, color='green',size=.2)+
    geom_vline(xintercept=-30, color='green',size=.2)+
    
  #coord_quickmap()+
  coord_fixed(1.3,xlim=c(-70,25),ylim=c(-40,40))+
  geom_polygon(data=world,aes(x=long,y=lat,group=group),col='darkgreen',fill='darkgreen')+
  geom_polygon(data=eez,aes(x=long,y=lat,group=group),color='lightblue',fill=NA,size=0.2)+
  geom_scatterpie(aes(x=longitude, y=latitude,r=radius/sf, group=region),
                    data=fish_rel3, cols=paste('freq',what.species,sep="."),color=NA, alpha=.9)   +
   #facet_wrap(~yrmon,ncol=2)
   # geom_scatterpie_legend((fish_rel3$radius/sf), x=-60, y=30, n=3, labeller=function(x) round(exp(x)/sf))  +

  theme(plot.margin=unit(c(1,1,1,1),"cm"),
        axis.text.x =element_text(colour="grey20",size=12,face="plain"),
        axis.text.y=element_text(colour="grey20",size=12,face="plain"),
        axis.title.y=element_text(size=15))+
  xlab("")+ylab("")


}
