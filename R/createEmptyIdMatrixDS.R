#' @title createEmptyIdMatrixDS
#' @description The function creates an empty matrix with unique id values as row names and a specified number of columns.
#' @param df A dataframe or a string representing a dataframe
#' @param idname A character string representing the column in the dataframe that contains the unique identifier for each observation
#' @param n_columns A numeric value or a string representing the number of columns in the empty matrix
#' @return An empty matrix with unique id values as row names and a specified number of columns
#' @export
createEmptyIdMatrixDS<- function(df, idname, n_columns){
  #matrix comes as string, vector as numbers
  if (is.character(df)){
    df <- eval(parse(text=df), envir = parent.frame())
  }

  if (is.character(n_columns)){
    n_columns <- eval(parse(text=n_columns), envir = parent.frame())
  }

  ids <- unique(df[, idname])

  matrix_zeros <- matrix(0, length(ids), n_columns)

  rownames(matrix_zeros) <- ids

  return(matrix_zeros)
}
