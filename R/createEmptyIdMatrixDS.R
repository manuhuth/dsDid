#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
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
