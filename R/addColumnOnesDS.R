#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
addColumnOnesDS <- function(X, columns){
  #matrix comes as string, vector as numbers
  X <- as.data.frame(eval(parse(text=X), envir = parent.frame()))

  matrix_ones <- cbind(rep(1, nrow(X)), X)

  colnames(matrix_ones) <- c("constant", columns)

  return(matrix_ones)

}
