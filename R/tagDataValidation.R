tagDataValidation <- function(input=rel_rec){
  
  # input <- rel_rec
  # Replace 0s with NAs
  
  val.df <- data.frame(longitude=rep(NA,length(input[,1])))
  val.df$relonx <- ifelse(is.na(input$relonx),0,1)
  val.df$relaty <- ifelse(is.na(input$relaty),0,1)
  val.df$rclonx <- ifelse(is.na(input$rclonx),0,1)
  val.df$rclaty <- ifelse(is.na(input$rclaty),0,1)
  val.df$redate <- ifelse(is.na(input$redate),0,1)
  val.df$rcdate <- ifelse(is.na(input$rcdate),0,1)
  val.df$relen <- ifelse(is.na(input$relen),0,1)
  val.df$rclen <- ifelse(is.na(input$rclen),0,1)
  
  
  val.df$valid.len <- ifelse(input$relen > input$rclen, 0,1)
  val.df$valid.timestamp <- ifelse(input$redate > input$rcdate, 0, 1)
  
  
  
  score <- apply(val.df[,c(1,2,3,4,9,10)],1,sum,na.rm=T)
  input$score <- score
  input
}
