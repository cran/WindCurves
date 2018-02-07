# Import a Power Curve image
ImportImage <- function(imagePath, rotate)
{
  if(missing(rotate))
  {
    rotate = 0
  }
  #img <- readbitmap::read.bitmap(imagePath)
  img <- read.bitmap(imagePath)
  op <- par(mar = c(0, 0, 0, 0))
  on.exit(par(op))
  plot.new()
  rasterImage(img, 0, 0, 1, 1, angle = rotate)
}
