#' A fitcurve function
#'
#' Fits the power curve with Weibull CDF, Logistic and user defined techniques
#' @param data as input data.frame with two columns, i.e., wind speed and wind power
#' @param MethodPath as path of a code for user defined curve fitting technique
#' @param MethodName as name of the user defined curve fitting technique
#' @import methods
#' @importFrom stats coef cor nls
#' @return fitted curves and corresponding discrete fitted values
#' @export
#' @examples
#' data(pcurves)
#' s <- pcurves$Speed
#' p <- pcurves$`Nordex N90`
#' da <- data.frame(s,p)
#' fitcurve(da)

fitcurve <- function(data, MethodPath, MethodName)
{
  if(!(hasArg(MethodName)))
  {
    MethodName <- "Proposed Method"
  }

  #da <- data.frame(s,p)
  da <- data

  s1 <- as.numeric(unlist(da[1]))
  s <- s1[ which(!da[2] == 0)]
  p1 <- as.numeric(unlist(da[2]))
  p <- p1[ which(!da[2] == 0)]
  if(length(s1)>length(s)){u <- length(s1)-length(s)}
  else(u <- 0)

  #########################################
  #
  #  Method 1: Weibull CDF
  #
  #########################################

  # Normalize the Power values
  norm_p <- (p - min(p))/(max(p) - min(p))


  # Selection of extreme points on a inclined linear part of the curve
  t2 <- which(abs(norm_p-mean(norm_p))==min(abs(norm_p-mean(norm_p))))
  t1 <- head(which(norm_p > 0),1)

  # calculte 'k' and 'C'
  k <- ((log(-log(1-norm_p[t2])))-(log(-log(1-norm_p[t1]))))/(log(s[t2])-log(s[t1]))
  c <- exp((k*log(s[t1]) - log(-log(1-norm_p[t1])))/k)

  # Weibull CDF
  F1 <- 1- exp(-(s/c)^k)

  # Denormalization of fitted power values
  M1 <- F1*(max(p)-min(p)) + min(p)

  if(length(s1)>length(s))
    {
      u <- length(s1)-length(s)
      M1 <- append(rep(0,u),M1)
  }

  # Print Model
  title <- "   Weibull CDF model"
  title1 <- "   -----------------"
  eq1 <- "   P = 1 - exp[-(S/C)^k]"
  eq2 <- "   where P -> Power and S -> Speed "
  cat(paste(title,title1,eq1,eq2, sep = '\n'))
  cat('\n\n')
  cat("    Shape (k)", '=', k, '\n')
  cat("    Scale (C)", '=', c, '\n')
  cat("   ===================================")
  cat('\n\n')

  #########################################
  #
  #  Method 2: Logistic Method
  #
  #########################################
  x <- s
  y <- p

  model.1 <- nls(y ~ SSlogis(x, ASym, xmid, scal))
  coef.sig<-coef(summary(model.1))[,1]
  M2<-coef.sig[1]/(1+exp((coef.sig[2]-x)/coef.sig[3]))
  if(length(s1)>length(s))
  {
    u <- length(s1)-length(s)
    M2 <- append(rep(0,u),M2)
  }

  # Print Model
  title <- "   Logistic Function model"
  title1 <- "   -----------------------"
  eq1 <- "   P = phi1/(1+exp((phi2-S)/phi3))"
  eq2 <- "   where P -> Power and S -> Speed "
  cat(paste(title,title1,eq1,eq2, sep = '\n'))
  cat('\n\n')
  for (i in 1:3) {
    cat("    phi", i, '=', coef.sig[i], '\n')
  }
  cat("   ===================================")
  cat('\n\n')

  if((hasArg(MethodPath)))
  {
    # to call functions from provided "MethodPath"
    dnew <- parse(text = MethodPath)
    dnew <- eval(dnew)
    M3 <- dnew$value(data)
    fitted <- data.frame(data,M1,M2,M3)
    names(fitted) <- c("Speed","Power","Weibull CDF","Logistic Function", MethodName)
  }
  else
  {
    fitted <- data.frame(data,M1,M2)
    names(fitted) <- c("Speed","Power","Weibull CDF","Logistic Function")
  }

  # Assign class "fitcurve" to returned object.
  class(fitted) <- "fitcurve"

  return(fitted)
}
