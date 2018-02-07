#' A function to capture Speed Vs Power discrete points from power curve image
#'
#' @param imagePath as Path of a power curve image
#' @param n as number of points to be captured from the curve image (default value is 15)
#' @import grid
#' @import readbitmap
#' @import methods
#' @importFrom utils head str
#' @importFrom grDevices dev.off png
#' @return data.frame with two columns, i.e., wind speed and wind power
#' @export
#' @examples
#' \donttest{
#' # to import image from system 'extdata' folder.
#' # user cab directly specify the path of the image in 'img2points()'.
#' imagePath <- system.file("extdata","powercurve.jpeg", package="WindCurves")
#' img2points(imagePath)}

img2points <- function(imagePath, n)
{
  if(!(hasArg(n)))
  {
    n <- 15
  }
  tilt <- 0
  value <- NULL
  grid.newpage()
  ImportImage(imagePath)
  cat(paste("Click on leftmost point on X axis and then enter the value of the point: ","\n"))
  xl <- do.click.value("cm", value)
  cat(paste("Click on rightmost point on X axis and then enter the value of the point: ","\n"))
  xr <- do.click.value("cm", value)
  cat(paste("Click on lowermost point on Y axis and then enter the value of the point: ","\n"))
  yd <- do.click.value("cm", value)
  cat(paste("Click on uppermost point on Y axis and then enter the value of the point: ","\n"))
  yu <- do.click.value("cm", value)

  if(atan((xr[2]-xl[2])/(xr[1]-xl[1]))!=0)
  {
    tilt <- rotate(xl[1],xr[1],xl[2],xr[2])
  }
  grid.newpage()
  ImportImage(imagePath = imagePath, rotate = tilt)

  pushViewport(viewport( x= unit(xl[1],"cm"), y= unit(yd[2],"cm"),  width=unit((xr[1]-xl[1]),"cm"), height=unit((yu[2]-yd[2]),"cm"),
                         xscale=c(xl[3], xr[3]), yscale=c(yd[3], yu[3]), just =c("left", "bottom")))
  gp <- get.gpar()
  #utils::str(gp)
  str(gp)
  grid.rect(gp = gpar(col="blue", fill="red", alpha=0.2))
  grid.xaxis()
  grid.yaxis()

  if(abs(tilt) > 2)
  {
    cat(paste("The image was tilted by",tilt, "Degrees.","Do you wish to resize the Scale?(Y/N)","\n"))
    f <- readline()
    if(f == "Y")
    {
      png("tiltedCurve.png")
      ImportImage(imagePath = imagePath, rotate = tilt)
      dev.off()
      grid.newpage()
      img2points(imagePath = "tiltedCurve.png", n = n)
    }
  }

  x_scale_factor <- (xr[3]-xl[3])/(xr[1]-xl[1])
  y_scale_factor <- (yu[3]-yd[3])/(yu[2]-yd[2])



  scaled_dot_x <- NULL
  scaled_dot_y <- NULL
  cat(paste("Select desired",n,"points on the curve: ", "\n"))
  for(i in 1:n)
  {
    dot <- do.click("cm")
    scaled_dot_x[i] <-  dot[1]*x_scale_factor+xl[3]
    scaled_dot_y[i] <-  dot[2]*y_scale_factor+yd[3]
  }
  scaled_dot_y[scaled_dot_y<0] <- 0
  dat <- list(x = scaled_dot_x, y = scaled_dot_y)
  dat <- data.frame(dat$x,dat$y)
  names(dat) <- c("Speed","Power")
  return(dat)
}


