#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
computeOddsDS <- function(name_propensities, name_dummy_g){
  #matrix comes as string, vector as numbers

  dummy_G <- eval(parse(text=name_dummy_g), envir = parent.frame())
  propensities <- eval(parse(text=name_propensities), envir = parent.frame())
  C <- 1 - dummy_G
  odds <- C * propensities / (1 - propensities)

  return(odds)

}
