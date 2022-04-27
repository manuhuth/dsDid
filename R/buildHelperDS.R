#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
buildHelperDS <- function(x){
  #matrix comes as string, vector as numbers

  x <- eval(parse(text=x), envir = parent.frame())
  return(x)
}
