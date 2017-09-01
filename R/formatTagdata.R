formatTagdata <- function(input=rel_rec){
  #Add useful vectors to the data
  #input <- rel_rec
  #Timestamps
  i <- sapply(input, is.factor)
  input[i] <- lapply(input[i], as.character)
  input$timestamp <- strptime(paste(input$date,input$time), format = "%Y-%m-%d %H:%M:%S") 
  input$rec_timestamp <- strptime(paste(input$rec_date,input$time), format = "%Y-%m-%d %H:%M:%S") 
  
  #Change zeros to NAs
  input$len <- ifelse(input$len==0,NA, input$len)
  input$rec_len <- ifelse(input$rec_len==0,NA, input$rec_len)
  
  #Electronic tag information
  wcs <- tagseries$tagnumidfrom[tagseries$tagtype == 'MiniPAT-348C' & tagseries$notes == "electronic pop-up"]:tagseries$tagnumidto[tagseries$tagtype == 'MiniPAT-348C' & tagseries$notes == "electronic pop-up"]
  dss <- tagseries$tagnumidfrom[tagseries$tagtype == 'SeaTag 3D PSAT' & tagseries$notes == "electronic pop-up. DESERT STAR"]:tagseries$tagnumidto[tagseries$tagtype == 'SeaTag 3D PSAT' & tagseries$notes == "electronic pop-up. DESERT STAR"]
  lok.2810 <- tagseries$tagnumidfrom[tagseries$tagtype == '2810' & tagseries$notes == "electronic internal. LOTEK 2810"]:tagseries$tagnumidto[tagseries$tagtype == '2810' & tagseries$notes == "electronic internal. LOTEK 2810"]
  lok.ageo <- tagseries$tagnumidfrom[tagseries$tagtype == 'ARCGEO-9' & tagseries$notes == "electronic internal. LOTEK arcgeo-9"]:tagseries$tagnumidto[tagseries$tagtype == 'ARCGEO-9' & tagseries$notes == "electronic internal. LOTEK arcgeo-9"]
 
  input$model[input$electronictagcode1 %in% lok.2810] <- 'Lotek-2810'
  input$model[input$electronictagcode1 %in% dss] <- 'DS-SeaTag-3D-PSAT'
  input$model[input$electronictagcode1 %in% wcs] <- 'MiniPAT-348C'
  input$model[input$electronictagcode1 %in% lok.ageo] <- 'ARCGEO-9'
  input$supplierserialnumber <- electronictags$supplierserialnumber[match(input$electronictagcode1,electronictags$electronictagcode)]
  input$argos                <- wc_codes$argos[match(input$electronictagcode1,wc_codes$aottp_id)]
  input
}
  