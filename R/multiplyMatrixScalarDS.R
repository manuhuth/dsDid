#' @title multiplyMatrixScalarDS
#' @title Multiply Matrix and Scalar
#' @description The function is used to multiply a matrix and a scalare.
#' @param matrix The matrix to be multiplied.  Must be a string with the name of the matrix on the server side.
#' @param scalar The scalar to be multiplied. Must be a single number.
#' @return A matrix representing the product of the two inputs
#' @export
multiplyMatrixScalarDS <- function(matrix, scalar) {
  matrix <- eval(parse(text = matrix), envir = parent.frame())
  scalar <- eval(parse(text = scalar), envir = parent.frame())


  product <- matrix * scalar
  return(product)
}
