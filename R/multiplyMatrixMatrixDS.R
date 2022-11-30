#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
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
