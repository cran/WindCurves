% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcurves.R
\docType{data}
\name{pcurves}
\alias{pcurves}
\title{Wind Turbine Power Curves}
\format{
An object of class \code{data.frame} with 25 rows and 7 columns.
}
\source{
\url{https://goo.gl/tD2JW6}
}
\usage{
data(pcurves)
}
\description{
Data on the power curves of wind turbine from
four major manufacturers: Siemens, Vestas, REpower and Nordex.
Represents wind turbine power output in 'kW' against wind speed in 'metres per second'.
}
\examples{
data(pcurves)
v <- pcurves$`Vestad V80`
}
\references{
Iain Staffell (2012)
\url{https://goo.gl/tD2JW6}
}
\keyword{curves}
\keyword{power}
