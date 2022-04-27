#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
AppendInfluenceDS <- function(df, influences, id_period_vector, column){
  #matrix comes as string, vector as numbers
  if (is.character(df)){
    df <- eval(parse(text=df), envir = parent.frame())
  }

  if (is.character(id_period_vector)){
    id_period_vector <- eval(parse(text=id_period_vector), envir = parent.frame())
  }

  if (is.character(column)){
    column <- eval(parse(text=column), envir = parent.frame())
  }

  id_vector <<- id_period_vector

  #df[id_period_vector, column] <- influences

  df[which( rownames(df) %in% id_period_vector), column] <- influences

  return(df)
}
