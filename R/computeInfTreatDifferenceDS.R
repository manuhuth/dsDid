#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
computeInfTreatDifferenceDS <- function(inf_1, inf_2){
  #matrix comes as string, vector as numbers

  inf_1 <- eval(parse(text=inf_1), envir = parent.frame())
  inf_2 <- eval(parse(text=inf_2), envir = parent.frame())

  diff <- inf_1 - inf_2

  return(diff)

}
