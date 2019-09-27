matchTagsA <- function(rels=releases,recs=recoveries,mtch = 'specimenid')

  {
   #rels <- releases; recs<- recoveries;mtch <- 'specimenid'
  # Matches releases and recoveries producing output in a 'short-format', e.g. columns have rec_longitude and rel_longitude
  mm <- match(rels[,mtch],recs[,mtch]) # add on some observations from release file.
  rels$rcnumtag1 <- recs$numtag1[mm]
  rels$rcnumtag2 <- recs$numtag2[mm]
  rels$rec_ctcolor1 <- recs$tagcolor1[mm]
  rels$rec_ctcolor2 <- recs$tagcolor2[mm]
  rels$rclonx <- recs$relonx[mm]
  rels$rclaty <- recs$relaty[mm]
  rels$rclen  <- recs$relen[mm]
  rels$rcgearcode <- recs$regearcode[mm]
  rels$rcdate <- recs$redate[mm]
  rels$rctime <- recs$retime[mm]
  rels$rcteam <- recs$reteam[mm]
  rels$recovered <-ifelse(is.na(mm),F,T)
  
  rels  
 }

