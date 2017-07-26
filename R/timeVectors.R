timeVectors <- function(input=rel_rec)
{
  input <- rel_rec
 #Month
 input$month<- months(as.POSIXlt(input$timestamp, format="%Y-%b-%d"))
 input$rec_month<- months(as.POSIXlt(input$rec_timestamp, format="%Y-%b-%d"))
 input$yrmon<-as.yearmon(input$date)
 input$rec_yrmon <- as.yearmon(input$rec_date)
 
  #Julian day
 input$jday     <- julian(as.POSIXlt(input$timestamp, format="%Y-%b-%d"),origin="2016-01-01")
 input$rec_jday <- julian(as.POSIXlt(input$rec_timestamp, format="%Y-%b-%d"),origin="2016-01-01")
 #Time at liberty
 input$days_at_liberty <- as.numeric(difftime(input$rec_timestamp,input$timestamp,units='days'))
 input
 }
  