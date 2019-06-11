fplot <- function (input = rel_rec, what.to.plot = 'days_at_liberty',what.species = 'SKJ',min.obs=25, max.obs=400)
  
{
  #input = rel_rec; what.to.plot = 'kms'; what.species = 'YFT'; max.obs=2000
  input1 <- input[input$speciescode %in% what.species,] 
  ggplot(data=input1,aes_string(x=what.to.plot,col="speciescode",group="speciescode",fill="speciescode"))+
  geom_density(alpha=0.9,adjust=.5)+
  xlim(min.obs,max.obs)+
    #xlab(what.to.plot)+
    facet_wrap(~ speciescode, ncol=1)+

  theme(plot.margin=unit(c(1,1,1,1),"cm"),strip.text = element_text(size=10),
        axis.text.x =element_text(colour="grey20",size=10,face="plain"),
        axis.text.y=element_text(colour="grey20",size=10,face="plain"),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10)
        )
  }

#fplot(input=rel_rec,what.to.plot='kms',what.species='SKJ',max.obs=8000)


#hellow world











