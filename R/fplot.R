fplot <- function (input = rel_rec, what.to.plot = 'days_at_liberty',what.species = 'SKJ',max.obs=400)
  
{
  #input = rel_rec; what.to.plot = 'kms'; what.species = 'SKJ'; max.obs=8000
  
  p <- ggplot(data=rel_rec[rel_rec$speciescode==what.species,],aes_string(x=what.to.plot,col="speciescode",group="speciescode",fill="speciescode"))+
  geom_density(alpha=0.9,adjust=.5)+
  xlim(0,max.obs)+xlab(what.to.plot)+facet_wrap(~ speciescode, ncol=1)
  print(summary(input[,what.to.plot]))
  p
  
}

#fplot(input=rel_rec,what.to.plot='kms',what.species='YFT',max.obs=5000)














