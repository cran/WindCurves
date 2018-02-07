#' A function to plot the curves fitted with fitcurve() function
#'
#' @param x is object returned by fitcurve() function
#' @param \dots Additional graphical parameters given to plot function.
#' @importFrom graphics axis box legend lines par plot plot.new rasterImage
#' @return Plot the curves fitted with fitcurve() function
#' @export
#' @examples
#' s <- pcurves$Speed
#' p <- pcurves$`Nordex N90`
#' da <- data.frame(s,p)
#' x <- fitcurve(da)
#' plot(x)
#'
plot.fitcurve <- function(x, ...)
{
  par(mar=c(4.2, 4.2, 0.4, 0.4))
  plot_colors <- c("blue","red","forestgreen","black")


  args <- list(...)

  if (length(args) == 0)
  {
    plot(unlist(x[1]),unlist(x[2]), type="o", col=plot_colors[1],pch=22, lty=2,
         axes=F, ann=T, xlab="Wind speed (m/s)",
         ylab="Power (kW)", cex.lab=1.4, lwd=2)

    # Make x axis tick marks without labels
    axis(1, las=1, cex.axis=1.1)

    # Plot y axis with smaller horizontal labels
    axis(2, las=1, cex.axis=1.1)

    # Create box around plot
    box()

    # Graph trucks with thicker red dashed line
    lines(unlist(x[1]), unlist(x[3]), type="o", lwd=2, col=plot_colors[2])

    # Graph suvs with thicker green dotted line
    lines(unlist(x[1]), unlist(x[4]), type="o", pch=22, lty=2, lwd=2, col=plot_colors[3])
    #lines(unlist(x[1]), unlist(x[5]), type="o", pch=22, lty=2, lwd=2, col=plot_colors[4])
    # Create a legend in the top-left corner that is slightly
    # smaller and has no border

    if(length(x) == 4)
    {
      legend("bottomright", c("Power curve","Weibull CDF Model", "Logistic Model"), cex=1.4, col=plot_colors,
             lty=c(2,1,2), lwd=2, bty="n");
    }
    else
    {
      lines(unlist(x[1]), unlist(x[5]), type="o", pch=22, lty=2, lwd=2, col=plot_colors[4])
      legend("bottomright", c("Power curve","Weibull CDF Model", "Logistic Model", names(x[5])), cex=1.4, col=plot_colors,
             lty=c(2,1,2), lwd=2, bty="n");
    }


  }

  else
  {
    plot(unlist(x[1]),unlist(x[2]), type="o", col=plot_colors[1], ...)

    # Make x axis tick marks without labels
    axis(1, las=1, cex.axis=1.1)

    # Plot y axis with smaller horizontal labels
    axis(2, las=1, cex.axis=1.1)

    # Create box around plot
    box()

    # Graph trucks with thicker red dashed line
    lines(unlist(x[1]), unlist(x[3]), type="o", col=plot_colors[2], ...)

    # Graph suvs with thicker green dotted line
    lines(unlist(x[1]), unlist(x[4]), type="o", col=plot_colors[3], ...)
    lines(unlist(x[1]), unlist(x[5]), type="o", col=plot_colors[4], ...)

    if(length(x) == 4)
    {
      legend("bottomright", c("Power curve","Weibull CDF Model", "Logistic Model"),  col=plot_colors, ...);
    }
    else
    {
      legend("bottomright", c("Power curve","Weibull CDF Model", "Logistic Model", names(x[5])),  col=plot_colors, ...);
    }

  }


}
