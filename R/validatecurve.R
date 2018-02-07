#' A Validate.curve function
#'
#' Compares the performance of curve fitting techniques fitted in fitcurve() function
#' @param x is object returned by fitcurve() function
#' @param MethodPath as path of a code for user defined error measure technique
#' @param MethodName as name of the user defined error measure technique
#' @return A comparison matrix in terms of various error measures.
#' @export
#' @examples
#' s <- pcurves$Speed
#' p <- pcurves$`Nordex N90`
#' da <- data.frame(s,p)
#' x <- fitcurve(da)
#' validate.curve(x)

validate.curve <- function(x, MethodPath, MethodName)
{
  E1 <- NULL
  E2 <- NULL
  actual <- as.numeric(unlist((x[2])))
  E3 <- 0
  if(!(hasArg(MethodName)))
  {
    MethodName <- "Error Measure"
  }
  for(i in 1:(length(x)-2))
  {
    modeled <- as.numeric(unlist(x[2+i]))

    # Absolute Error
    # AE <- abs(modeled - actual)
    # Relative Error
    # RE <- (modeled - actual)/actual * 100

    # MAE
    MAE <- mean(abs(modeled - actual))

    # MAPE
    modeled_n <- modeled[ which(!modeled == 0)]
    actual_n <- actual[ which(!modeled == 0)]
    MAPE <-  mean(abs(modeled_n - actual_n)* 100/modeled_n)

    # RMSE
    RMSE <- sqrt(mean((modeled - actual)^2))

    # R squared - Coefficient of Determination
    R2 <- 1 - (sum((modeled - actual)^2)/sum((actual-mean(actual))^2))

    # Correlation Coefficient
    COR <- cor(modeled, actual)

    if((hasArg(MethodPath)))
    {
      # to call functions from provided "MethodPath"
      dnew <- parse(text = MethodPath)
      dnew <- eval(dnew)
      Error_New <- dnew$value(modeled, actual)

    assign(paste("E", i, sep = ""), c(RMSE,MAE,MAPE,R2,COR,Error_New))
    }
    else
    {
      assign(paste("E", i, sep = ""), c(RMSE,MAE,MAPE,R2,COR))
    }
  }

  if((hasArg(MethodPath)))
  {
    errM <- c("RMSE","MAE","MAPE","R2","COR",MethodName)
    if(length(E3) == length(E2))
    {
      matrix <- data.frame(errM,E1,E2,E3)
    }
    else
      matrix <- data.frame(errM,E1,E2)

    names(matrix) <- c("Metrics", names(x)[3:length(x)])
    return(matrix)

  }
  else
  {
    errM <- c("RMSE","MAE","MAPE","R2","COR")
    if(length(E3) == length(E2))
    {
      matrix <- data.frame(errM,E1,E2,E3)
    }
    else
      matrix <- data.frame(errM,E1,E2)

    names(matrix) <- c("Metrics", names(x)[3:length(x)])
    return(matrix)
  }


}
