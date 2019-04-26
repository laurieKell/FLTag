timeVectors <- function(input=rel_rec,orig.date="1940-01-01")
{

 #input<- rel_rec
 
  orig.date<-as.POSIXct(orig.date)
  
  i <- sapply(input, is.factor)
  input[i] <- lapply(input[i], as.character)
  
  
 #Timestamps
 
 input$retimestamp <- as.POSIXct(strptime(paste(input$redate,input$retime), format = "%Y-%m-%d %H:%M:%S")) 
 input$rctimestamp <- as.POSIXct(strptime(paste(input$rcdate,input$rctime), format = "%Y-%m-%d %H:%M:%S")) 
 
 #Dates
 input$redate <-     as.POSIXct(strptime(input$redate, format = "%Y-%m-%d"))
 input$rcdate <- as.POSIXct(strptime(input$rcdate, format = "%Y-%m-%d")) 
 
 
 #Month
 input$remonth<- months(as.POSIXct(input$redate, format="%Y-%b-%d"))
 input$rcmonth<- months(as.POSIXct(input$rcdate, format="%Y-%b-%d"))
 input$reyear <- year(as.POSIXct(input$redate, format="%Y-%b-%d"))
 input$rcyear <- year(as.POSIXct(input$rcdate, format="%Y-%b-%d"))
 input$reyrmon<-as.yearmon(input$redate)
 input$rcyrmon <- as.yearmon(input$rcdate)
 
  #Julian day
 input$rejday     <- julian(as.POSIXct(input$redate, format="%Y-%b-%d"),origin=orig.date)
 input$rcjday     <- julian(as.POSIXct(input$rcdate, format="%Y-%b-%d"),origin=orig.date)
 #Time at liberty
 input$days_at_liberty <- as.numeric(difftime(input$rcdate,input$redate,units='days'))
 input
 }

