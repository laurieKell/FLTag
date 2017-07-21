timeVectors <- function(rel_rec=rel_rec)
{
  # rel_rec <- rel_rec
 #Month
 rel_rec$month<- month(as.POSIXlt(rel_rec$timestamp, format="%Y-%b-%d"))
 rel_rec$rec_month<- month(as.POSIXlt(rel_rec$rec_timestamp, format="%Y-%b-%d"))
 #Week
 rel_rec$week<- week(as.POSIXlt(rel_rec$timestamp, format="%Y-%b-%d"))
 rel_rec$rec_week<- week(as.POSIXlt(rel_rec$rec_timestamp, format="%Y-%b-%d"))
 #Julian day
 rel_rec$jday     <- julian(as.POSIXlt(rel_rec$timestamp, format="%Y-%b-%d"),origin="2016-01-01")
 rel_rec$rec_jday <- julian(as.POSIXlt(rel_rec$rec_timestamp, format="%Y-%b-%d"),origin="2016-01-01")
 #Time at liberty
 rel_rec$days_at_liberty <- as.numeric(difftime(rel_rec$rec_timestamp,rel_rec$timestamp,units='days'))
 rel_rec
 }
  