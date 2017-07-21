formatTagdata <- function(rel_rec=rel_rec){
  
  i <- sapply(rel_rec, is.factor)
  rel_rec[i] <- lapply(rel_rec[i], as.character)
  rel_rec$timestamp <- strptime(paste(rel_rec$date,rel_rec$time), format = "%Y-%m-%d %H:%M:%S") 
  rel_rec$rec_timestamp <- strptime(paste(rel_rec$rec_date,rel_rec$time), format = "%Y-%m-%d %H:%M:%S") 
  rel_rec
}
  