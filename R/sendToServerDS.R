#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
sendToServerDS <- function(x) {
  # matrix comes as string, vector as numbers

  if (!(length(x) == 1 & is.numeric(x))) {
    stop("x must be a single number.")
  }

  return(x)
}
