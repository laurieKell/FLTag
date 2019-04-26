matchTagsA <- function(rels=releases,recs=recoveries,mtch = 'specimenid')

  {
   #rels <- releases; recs<- recoveries;mtch <- 'specimenid'
  # Matches releases and recoveries producing output in a 'short-format', e.g. columns have rec_longitude and rel_longitude
  mm <- match(rels[,mtch],recs[,mtch]) # add on some observations from release file.
  rels$rec_ctcode1 <- recs$ctcode1[mm]
  rels$rec_ctcode2 <- recs$ctcode2[mm]
  rels$rec_ctcolor1 <- recs$ctcolor1[mm]
  rels$rec_ctcolor2 <- recs$ctcolor2[mm]
  rels$rec_longitude <- recs$longitude[mm]
  rels$rec_latitude <- recs$latitude[mm]
  rels$rec_len  <- recs$len[mm]
  rels$rec_gearcode <- recs$gearcode[mm]
  rels$rec_date <- recs$date[mm]
  rels$rec_time <- recs$time[mm]
  rels$rec_team <- recs$team[mm]
  rels$recovered <-ifelse(is.na(mm),F,T)
  
  rels  
 }

