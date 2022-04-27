#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
multiplierBootstrapDS <- function(matrix, n_iterations){
  #matrix comes as string, vector as numbers
  if (is.character(matrix)){
    matrix <- as.matrix(eval(parse(text=matrix), envir = parent.frame()))
  }

  if (is.character(n_iterations)){
    n_iterations <- as.matrix(eval(parse(text=n_iterations), envir = parent.frame()))
  }

  matrix_out <- BMisc::multiplier_bootstrap(matrix, n_iterations) * nrow(matrix)

  return(matrix_out)
}
