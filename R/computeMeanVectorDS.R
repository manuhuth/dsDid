#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
computeMeanVectorDS <- function(x, object_for_length){

  x <- eval(parse(text=x), envir = parent.frame())

  if (is.null(object_for_length)){
    length_x <- length(x)
  } else{
    object_for_length <- eval(parse(text=object_for_length), envir = parent.frame())
    length_x <- nrow(object_for_length)
  }
  print(length_x)
  print(x)
  mean_x <- mean(x)

  return(rep(mean_x, length_x))

}
