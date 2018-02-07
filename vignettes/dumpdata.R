error <-
function(a,b)
{
d <- (var(a) - var(b)) * 100/ var(a)
d <- as.numeric(d)
return(d)
}
