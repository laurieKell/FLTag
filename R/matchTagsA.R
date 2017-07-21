matchTagsA <- function(rels=releases,recs=recoveries,mtch = 'specimenid')

  {
  # rels <- releases; recs<- recoveries
  # Matches releases and recoveries producing output in a 'short-format', e.g. columns have rec_longitude and rel_longitude
  mm <- match(rels[,mtch],recs[,mtch]) # add on some observations from release file.
  rels$rec_longitude <- recs$longitude[mm]
  rels$rec_latitude <- recs$latitude[mm]
  rels$rec_len  <- recs$len[mm]
  rels$rec_gearcode <- recs$gearcode[mm]
  rels$rec_date <- recs$date[mm]
  rels$rec_time <- recs$time[mm]
  rels  
 }

