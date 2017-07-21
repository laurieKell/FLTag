tagDataValidation <- function(rel_rec=rel_rec){
  
  val.df <- data.frame(longitude=rep(NA,length(rel_rec[,1])))
  val.df$longitude <- ifelse(is.na(rel_rec$longitude),0,1)
  val.df$latitude <- ifelse(is.na(rel_rec$latitude),0,1)
  val.df$rec_longitude <- ifelse(is.na(rel_rec$rec_longitude),0,1)
  val.df$rec_latitude <- ifelse(is.na(rel_rec$rec_latitude),0,1)
  val.df$timestamp <- ifelse(is.na(rel_rec$timestamp),0,1)
  val.df$rec_timestamp <- ifelse(is.na(rel_rec$rec_timestamp),0,1)
  val.df$len <- ifelse(is.na(rel_rec$len),0,1)
  val.df$rec_len <- ifelse(is.na(rel_rec$rec_len),0,1)
  val.df$valid.len <- ifelse(rel_rec$len > rel_rec$rec_len, 0,1)
  val.df$timestamp <- ifelse(is.na(rel_rec$timestamp),0,1)
  val.df$rec_timestamp <- ifelse(is.na(rel_rec$rec_timestamp),0,1)
  val.df$valid.timestamp <- ifelse(rel_rec$timestamp > rel_rec$rec_timestamp, 0, 1)
  score <- apply(val.df,1,sum)
  rel_rec$score <- score
  rel_rec
}