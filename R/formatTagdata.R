formatTagdata <- function(input=rel_rec){
  
  i <- sapply(input, is.factor)
  input[i] <- lapply(input[i], as.character)
  input$timestamp <- strptime(paste(input$date,input$time), format = "%Y-%m-%d %H:%M:%S") 
  input$rec_timestamp <- strptime(paste(input$rec_date,input$time), format = "%Y-%m-%d %H:%M:%S") 
  input
}
  