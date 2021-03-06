% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validatecurve.R
\name{validate.curve}
\alias{validate.curve}
\title{A Validate.curve function}
\usage{
validate.curve(x, MethodPath, MethodName)
}
\arguments{
\item{x}{is object returned by fitcurve() function}

\item{MethodPath}{as path of a code for user defined error measure technique}

\item{MethodName}{as name of the user defined error measure technique}
}
\value{
A comparison matrix in terms of various error measures.
}
\description{
Compares the performance of curve fitting techniques fitted in fitcurve() function
}
\examples{
s <- pcurves$Speed
p <- pcurves$`Nordex N90`
da <- data.frame(s,p)
x <- fitcurve(da)
validate.curve(x)
}
