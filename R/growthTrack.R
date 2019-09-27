growthTrack <- function(input = rel_rec,what.species = 'BET',what.size=1)
  
{
  
  
  #input = rel_rec; what.species = c("BET","YFT");what.size=.1
    ggplot() +
   geom_segment(data=input[input$speciescode %in% what.species,],
                 aes(x=redate,y=relen,xend=rcdate,yend=rclen,
                     group=specimenid,color=speciescode),
              arrow = arrow(length = unit(0.1,"cm")),size=what.size,alpha=1) +
      #geom_point(data=input[input$speciescode %in% what.species,],
       # aes(x=redate,y=relen,group=specimenid),color='blue',size=.01,alpha=.1) +
      #geom_point(data=input[input$speciescode %in% what.species,],
       # aes(x=rcdate,y=rclen,group=specimenid),color='green',size=.01,alpha=.5) +
     
     # geom_point(data=input[input$speciescode %in% what.species,],
      #  aes(x=rcdate,y=jitter(25,amount=10),group=specimenid),color='yellow',size=.01,alpha=.5) +
      
      
       xlim(as.POSIXct("2016-06-01"),as.POSIXct("2019-10-31"))  +
      
      
      
  facet_wrap(~ speciescode, ncol=1) +
    theme(plot.margin=unit(c(2,2,1,1),"cm"),
          axis.text.x = element_text(colour="grey20",size=9,face="plain"),
          axis.text.y=element_text(colour="grey20",size=10,face="plain"),
          axis.title.y=element_text(size=10))+
    theme_grey(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("")+ylab("Length(cms)")
  
  
} 


