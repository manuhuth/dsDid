#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
generateNotYetTreatedDS <- function(df, name_variable, t, g){
  df <- eval(parse(text=df), envir = parent.frame())

  if (t>=g){
    max_min <- max(t,g)
  } else {
    max_min <- min(t,g)
  }
  #use only observations that are from the g period, or after or never treated
  df_subset <- df[which( (df[, name_variable] == g) | (df[, name_variable] == 0) | (df[, name_variable] > max_min) ), ]
                          #treated in g                   #never tretaed                #not yet treatd

  df_subset[which((df_subset[, name_variable] > max_min) & (df_subset[, name_variable] != g)), name_variable] <- 0
  return(df_subset)

}
