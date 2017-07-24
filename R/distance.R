distance <- 
  function (rec_longitude=rec_longitude, rec_latitude=rec_latitude, rel_longitude=longitude, rel_latitude = latitude) 
  {
    # calculate distance between tag-release and tag-recovery
    pd <- pi/180
    a1 <- sin(((rel_latitude - rec_latitude) * pd)/2)
    a2 <- cos(rec_latitude * pd)
    a3 <- cos(rel_latitude * pd)
    a4 <- sin(((rel_longitude - rec_longitude) * pd)/2)
    a <- a1 * a1 + a2 * a3 * a4 * a4
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    return(6371 * c)
  }


