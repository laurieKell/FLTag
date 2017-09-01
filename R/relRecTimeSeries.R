relRecTimeSeries <- function(input=rel_rec, what.species = c('BET','LTA','SKJ','YFT'))
{
#input <- rel_rec
  
ts1 <- melt(table(speciescode=input$speciescode,date=input$date))
ts2 <- melt(table(speciescode=input$speciescode,rec_date=input$rec_date))
names(ts1)[3]<-'value'
names(ts2)[3]<-'value'
ts1$date<-as.POSIXct(as.character(ts1$date))
ts2$date<-as.POSIXct(as.character(ts2$rec_date))

ts1$svalue <- sqrt(ts1$value)
ts2$svalue <- sqrt(ts2$value)

#plot.title  <-  'Total releases (green) and recoveries (red) by species over time'
#plot.subtitle <- '(AOTTP)'
ggplot(data=ts1[ts1$speciescode %in% what.species,], aes(x=date, y=svalue)) +
  geom_bar(data=ts1[ts1$speciescode %in% what.species,],color='green',fill='green',stat="identity", position=position_dodge(),alpha=.5)         +
  facet_wrap(~speciescode,ncol=1)+
  geom_bar(data=ts2[ts2$speciescode %in% what.species,],color='red',fill='red',stat="identity", position=position_dodge(),alpha=.5)         +
  scale_y_continuous() +
  #scale_x_date(breaks = "1 month", minor_breaks = "1 month", labels=date_format("%m"))  +
  xlab("") + ylab("sqrt(Frequency)" )
  #ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) + 
  #theme(plot.title = element_text(face = "bold",size = 16,colour="black"))
}







