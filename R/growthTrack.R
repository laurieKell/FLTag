growthTrack <- function(input = rel_rec,what.species = 'SKJ')
  
{
  #input = rel_rec[rel_rec$score==6,]; what.species = c("BET","LTA","SKJ","YFT")
    ggplot() +
   geom_segment(data=input[input$speciescode %in% what.species,],
                 aes(x=date,y=len,xend=rec_date,yend=rec_len,group=specimenid,color=speciescode),
              arrow = arrow(length = unit(0.2,"cm")),size=.5,alpha=.25) +
  facet_wrap(~ speciescode, ncol=2) +
    theme(plot.margin=unit(c(1,1,1,1),"cm"),
          axis.text.x =element_text(colour="grey20",size=9,face="plain"),
          axis.text.y=element_text(colour="grey20",size=9,face="plain"),
          axis.title.y=element_text(size=10))+
    xlab("")+ylab("Length(cms)")
  
  
}
