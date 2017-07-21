matchTagsB <- function(rels=releases,recs=recoveries)
  
{
  # Matches releases and recoveries producing output with recovery point after release for each tag. Needed for plotting arrows ##
  
  rels <- releases
  recs <- recoveries
  
  recs$speciescode <- releases$speciescode[match(recoveries$ctcode1,releases$ctcode1)]
  all<- merge(data.frame(recs[,c("specimenid","ctcode1","ctcode2","speciescode","date","latitude","longitude","len","gearcode")]),
              data.frame(rels[,c("specimenid","ctcode1","ctcode2","speciescode","date","latitude","longitude","len","gearcode")]),all=T)
  
  x <- table(paste(all$ctcode1,all$speciescode));
  x <- x[x==2]
  
  all <- all[paste(all$ctcode1,all$speciescode) %in% as.vector(names(x)),]
  all<- all[all$speciescode %in% c('BET','LTA','SKJ','YFT'),]
  all<- orderBy(~ctcode1+speciescode+date,data=all)
  #prob.tags <- c(7124,10410,10585,10374,1093,1013,1039,2006)
  #all <- all[!c(all$ctcode1 %in% prob.tags),]
  
  #d_len <- tapply(all$len,all$ctcode,diff)
  #reliable_len <- names(d_len)[d_len >= 0]
  all$status <- rep(c('release','recovery'),length(all[,1])/2)
  #all<- all[all$ctcode1 %in% reliable_len,]
  
  #tmp <- as.POSIXlt(all$date[all$status == 'release'], format = "%Y-%b-%d") # vector for julian day
  #all$rday[all$status == 'release'] <- tmp$yday
  #all$rday[all$status == 'recovery'] <- tmp$yday
  
  all

}


ggplot(all[all$speciescode %in% c('BET','LTA','SKJ','YFT'),], 
       aes(x=len,group=status,color=status,fill=status)) + geom_density(alpha=0.5)+
  xlim(30,120)+
  facet_wrap(~ speciescode, ncol=2)

library(ggmap)


wAf <- c(-25,0)
wAfMap <- get_map(location=wAf,source='google',maptype='satellite',crop=FALSE,zoom=3)
dd <- all[all$speciescode %in% c('BET','LTA','SKJ','YFT'),]                                                                                          
ggmap(wAfMap) +
  facet_wrap(~ speciescode, ncol=2) +
  geom_path(data=dd,aes(x=longitude,y=latitude,group=specimenid,color=speciescode),
            arrow = arrow(length = unit(0.1,"cm")),size=.25,alpha=.5) +
  geom_point(data=releases[releases$speciescode %in% c('BET','LTA','SKJ','YFT'), ],
             aes(x=longitude,y=latitude,group=specimenid,size=.00001),color='grey',size=.00001) +
  xlab("")+ylab("")







