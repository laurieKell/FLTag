relRecTimeSeries <- function(input=rel_rec, what.species = c('BET','LTA','SKJ','YFT'))
{
 #input <- rel_rec
  #input <- input[input$tagseeding==0,] # Tagseeding or not 
  #what.species <- c('BET','LTA','SKJ','YFT')
  #ts1 <- melt(table(speciescode=input$speciescode,yrmon=input$yrmon))
  #ts2 <- melt(table(speciescode=input$speciescode,yrmon=input$rec_yrmon))
  
  ts1 <- melt(table(speciescode=input$speciescode,ndate=input$redate))
  ts2 <- melt(table(speciescode=input$speciescode,ndate=input$rcdate))
  
  
  names(ts1)[3]<-'value'
  names(ts2)[3]<-'value'
  
  ts1$svalue <- sqrt(ts1$value)
  ts2$svalue <- sqrt(ts2$value)
  ts1$svalue <- ifelse(ts1$svalue==0,NA,ts1$svalue)
  ts2$svalue <- ifelse(ts2$svalue==0,NA,ts2$svalue)
  
  #plot.title  <-  'Total releases (green) and recoveries (red) by species over time'
  #plot.subtitle <- '(AOTTP)'
  ts1<-orderBy(~ndate+speciescode,data=ts1)
  ts2<-orderBy(~ndate+speciescode,data=ts2)
  
  #ts1$ndate <- as.POSIXct(strptime(paste('15',ts1$yrmon),format="%d %B %Y"))
  #ts2$ndate <- as.POSIXct(strptime(paste('15',ts2$yrmon),format="%d %B %Y"))
  
  
  ts1$ndate <- as.POSIXct(strptime(ts1$ndate,format="%Y-%m-%d"))
  ts2$ndate <- as.POSIXct(strptime(ts2$ndate,format="%Y-%m-%d"))
  
  ts1$ndate1 <- as.Date(ts1$ndate)
  ts2$ndate1 <- as.Date(ts2$ndate)
  
  ggplot(data=ts1[ts1$speciescode %in% what.species,], aes(x=ndate1, y=svalue)) +
    facet_wrap(~speciescode,ncol=1)+
    geom_bar(data=ts1[ts1$speciescode %in% what.species,],color='blue',fill='blue',stat="identity", position=position_dodge(),alpha=.25)         +
    geom_bar(data=ts2[ts2$speciescode %in% what.species,],color='red',fill='red',stat="identity", position=position_dodge(),alpha=.25)         +
    scale_y_continuous() +
    scale_x_date(breaks = "1 month", minor_breaks = "1 month", labels=date_format("%Y-%m-%d"))  +
    xlab("") + ylab("sqrt(Frequency)") +
    theme(plot.margin=unit(c(1,1,1,1),"cm"),strip.text = element_text(size=10),
          axis.text.x =element_text(colour="grey20",size=10,face="plain",angle=90,hjust=1),
          axis.text.y=element_text(colour="grey20",size=10,face="plain"),
          axis.title.y=element_text(size=10))
  #ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) + 
  #theme(plot.title = element_text(face = "bold",size = 16,colour="black"))
  
  
}

