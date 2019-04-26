growthTrack <- function(input = rel_rec,what.species = 'BET',what.size=1)
  
{
  #input = rel_rec[rel_rec$score==6,]; what.species = c("BET","LTA","SKJ","YFT")
    ggplot() +
   geom_segment(data=input[input$speciescode %in% what.species,],
                 aes(x=timestamp,y=relen,xend=rctimestamp,yend=rclen,group=specimenid,color=speciescode),
              arrow = arrow(length = unit(0.2,"cm")),size=what.size,alpha=.8) +
  facet_wrap(~ speciescode, ncol=3) +
    theme(plot.margin=unit(c(2,2,1,1),"cm"),
          axis.text.x = element_text(colour="grey20",size=9,face="plain"),
          axis.text.y=element_text(colour="grey20",size=10,face="plain"),
          axis.title.y=element_text(size=10))+
    theme_grey(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("")+ylab("Length(cms)")
  
  
} 


