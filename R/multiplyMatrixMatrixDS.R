#' @title multiplyMatrixMatrixDS
#' @description The function multiplies two matrices and returns the product matrix. At least one of the matrices must be from the server-side.
#' @param matrix1 A matrix or a string representing a matrix
#' @param matrix2 A matrix or a string representing a matrix
#' @param nrow1 The number of rows for the first matrix, used only if the first matrix is not a string
#' @param ncol1 The number of columns for the first matrix, used only if the first matrix is not a string
#' @param nrow2 The number of rows for the second matrix, used only if the second matrix is not a string
#' @param ncol2 The number of columns for the second matrix, used only if the second matrix is not a string
#' @return A matrix representing the product of the two input matrices
#' @export
multiplyMatrixMatrixDS <- function(matrix1, matrix2, nrow1=NULL, ncol1=NULL,
                                nrow2=NULL, ncol2=NULL){

  if (!(is.character(matrix1) | is.character(matrix2))){
    stop("At least one of the two matrices must be from the server-side.")
  }


  #matrix comes as string, vector as numbers
  if (is.character(matrix1)){
    matrix1 <- eval(parse(text=matrix1), envir = parent.frame())
  } else if (is.null(nrow1) & is.null(ncol1)) {
    matrix1 <- as.matrix(matrix1)
  }
  else{
    matrix1 <- matrix(matrix1, nrow=nrow1, ncol=ncol1)
  }

  if (is.character(matrix2)){
    matrix2 <- eval(parse(text=matrix2), envir = parent.frame())
  } else if (is.null(nrow2) & is.null(ncol2)) {
    matrix2 <- as.matrix(matrix2)
  }
    else {
    matrix2 <- matrix(matrix2, nrow=nrow2, ncol=ncol2)
  }

  product <- as.matrix(matrix1) %*% as.matrix(matrix2)



  return(product)
}
