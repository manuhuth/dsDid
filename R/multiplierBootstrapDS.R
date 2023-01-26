#' @title multiplierBootstrapDS
#' @description The function performs the multiplier bootstrap on a given matrix and number of iterations, and returns the bootstrapped matrix.
#' @param matrix A matrix or a string representing a matrix
#' @param n_iterations A numeric value or a string representing the number of iterations for the bootstrap
#' @return A matrix that is the result of the multiplier bootstrap
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
