#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
multiplyMatrixVectorDS <- function(vector, matrix){
  #matrix comes as string, vector as numbers

  if (is.character(matrix)){
    matrix <- eval(parse(text=matrix), envir = parent.frame())
  }

  if (is.character(vector)){
    vector <- eval(parse(text=vector), envir = parent.frame())
  }

  product <- as.matrix(matrix) %*% as.matrix(vector)

  return(product)

}
