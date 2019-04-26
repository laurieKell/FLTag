distance <- 
  function (rclonx=rclonx, rclaty=rclaty, relonx=relonx, relaty = relaty) 
  {
    # calculate distance between tag-release and tag-recovery
    pd <- pi/180
    a1 <- sin(((relaty - rclaty) * pd)/2)
    a2 <- cos(rclaty * pd)
    a3 <- cos(relaty * pd)
    a4 <- sin(((relonx - rclonx) * pd)/2)
    a <- a1 * a1 + a2 * a3 * a4 * a4
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    return(6371 * c)
  }


