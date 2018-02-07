rotate <- function(x1,x2,y1,y2)
{
  a <- atan((y2-y1)/(x2-x1))
  a <- (a * 180) / (pi)
  a <- a*(-1)
  return(a)
}
