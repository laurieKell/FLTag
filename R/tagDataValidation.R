tagDataValidation <- function(input=rel_rec){
  
  #input <- rel_rec
  val.df <- data.frame(longitude=rep(NA,length(input[,1])))
  val.df$longitude <- ifelse(is.na(input$longitude),0,1)
  val.df$latitude <- ifelse(is.na(input$latitude),0,1)
  val.df$rec_longitude <- ifelse(is.na(input$rec_longitude),0,1)
  val.df$rec_latitude <- ifelse(is.na(input$rec_latitude),0,1)
  val.df$timestamp <- ifelse(is.na(input$timestamp),0,1)
  val.df$rec_timestamp <- ifelse(is.na(input$rec_timestamp),0,1)
  val.df$len <- ifelse(is.na(input$len),0,1)
  val.df$rec_len <- ifelse(is.na(input$rec_len),0,1)
  val.df$valid.len <- ifelse(input$len > input$rec_len, 0,1)
  val.df$timestamp <- ifelse(is.na(input$timestamp),0,1)
  val.df$rec_timestamp <- ifelse(is.na(input$rec_timestamp),0,1)
  val.df$valid.timestamp <- ifelse(input$timestamp > input$rec_timestamp, 0, 1)
  score <- apply(val.df,1,sum,na.rm=T)
  input$score <- score
  input
}