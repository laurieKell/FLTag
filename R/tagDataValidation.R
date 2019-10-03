tagDataValidation <- function(input=rel_rec){
  
  # input <- rel_rec_all
  # Replace 0s with NAs
  
  val.df <- data.frame(relonx=rep(NA,length(input$relonx)))
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
  
  which.cols <-c("relonx","relaty","rclonx","rclaty","redate","rcdate","relen","rclen")
  
  score <- apply(val.df[,which.cols],1,sum,na.rm=T)
  input$reloc.v <- apply(val.df[,which.cols[c(1,2)]],1,sum,na.rm=T) # if reloc.v=2 then you have the release loc
  input$rcloc.v <- apply(val.df[,which.cols[c(3,4)]],1,sum,na.rm=T) # if rcloc.v=2 then you have the recovery loc
  input$redate.v <- val.df[,"redate"]
  input$rcdate.v <- val.df[,"rcdate"]
  input$len.v <- apply(val.df[,which.cols[c(7,8)]],1,sum,na.rm=T)  # if len.v = 2 then you have both
  input$score.v <- score
  
  
  input
}
