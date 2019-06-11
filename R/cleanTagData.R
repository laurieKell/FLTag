cleanTagData <- function(input=rel_rec)
{
  
  # Function to clean up character strings
  
  #input <- rel_rec
  
  # input$tsplacename <- gsub('MarchÃ© Central Au Poisson','Central Fish Market',input$tsplacename)
  # input$tsplacename <- gsub('MarchÃ© Central AU Poisson','Central Fish Market',input$tsplacename)
   input$tsplacename <- gsub('Marche Au Poisson','Central Fish Market',input$tsplacename)
  input$tsplacename <- gsub('Marche Centre Au Poisson','Central Fish Market',input$tsplacename)
  
  
  input$tsplacename <- gsub('Au Port Abidjan','Abidjan Port',input$tsplacename)
  input$tsplacename <- gsub('Port Abidjan','Abidjan Port',input$tsplacename)
  
  input$tsplacename <- gsub('Au Port ABIDJAN','Abidjan Port',input$tsplacename)
  input$tsplacename <- gsub('Port ABIDJAN','Abidjan Port',input$tsplacename)

  
  input$tsplacename <- gsub('Kayar','Kayare',input$tsplacename)
  input$tsplacename <- gsub('Kayaree','Kayare',input$tsplacename)
  input$tsplacename <- gsub('Port Autonome De Dakar','Autonomous Port of Dakar',input$tsplacename)

  input
  
  
  
  
  
  
}