#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
genDfSubsetDS <- function(df, name_variable, value, include_zero=TRUE){
  df <- eval(parse(text=df), envir = parent.frame())
  value <- eval(parse(text=value), envir = parent.frame())

  if (isTRUE(include_zero)){
    df_subset <- df[which((df[, name_variable] == value) | (df[, name_variable] == 0) ), ]
  } else{
    df_subset <- df[which((df[, name_variable] == value) ), ]
  }
  return(df_subset)

}
