growthTrack <- function(input = rel_rec,what.species = 'SKJ',what.size=1)
  
{
  #input = rel_rec[rel_rec$score==6,]; what.species = c("BET","LTA","SKJ","YFT")
    ggplot() +
   geom_segment(data=input[input$speciescode %in% what.species,],
                 aes(x=date,y=len,xend=rec_date,yend=rec_len,group=specimenid,color=speciescode),
              arrow = arrow(length = unit(0.2,"cm")),size=what.size,alpha=.8) +
  facet_wrap(~ speciescode, ncol=2) +
    theme(plot.margin=unit(c(2,2,1,1),"cm"),
          axis.text.x = element_text(colour="grey20",size=9,face="plain"),
          axis.text.y=element_text(colour="grey20",size=10,face="plain"),
          axis.title.y=element_text(size=15))+
    theme_grey(base_size = 20) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("")+ylab("Length(cms)")
  
  
} 


