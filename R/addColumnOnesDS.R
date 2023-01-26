#' @title addColumnOnesDS
#' @description The function adds a column of ones to a dataframe and returns the new dataframe.
#' @param X A string representing a dataframe
#' @param columns A character vector of column names for the new dataframe
#' @return A dataframe with a column of ones added
#' @export
addColumnOnesDS <- function(X, columns){

  X <- as.data.frame(eval(parse(text=X), envir = parent.frame()))

  matrix_ones <- cbind(rep(1, nrow(X)), X)

  colnames(matrix_ones) <- c("constant", columns)

  return(matrix_ones)

}
