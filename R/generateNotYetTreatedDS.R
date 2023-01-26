#' @title generateNotYetTreatedDS
#' @description The function generates a subset of a dataframe based on a specified variable and value, while only including observations that are from the g period or after, or have never been treated.
#' @param df A dataframe or a string representing a dataframe
#' @param name_variable A character string representing the column in the dataframe to subset on
#' @param t An integer representing the minimum period value
#' @param g An integer representing the current period
#' @return A subset of the dataframe with rows that match the specified conditions
#' @export
generateNotYetTreatedDS <- function(df, name_variable, t, g){
  df <- eval(parse(text=df), envir = parent.frame())

  max_min <- t
  #use only observations that are from the g period, or after or never treated
  df_subset <- df[which( (df[, name_variable] == g) | (df[, name_variable] == 0) | (df[, name_variable] > max_min) ), ]
                          #treated in g                   #never tretaed                #not yet treatd

  df_subset[which((df_subset[, name_variable] > max_min) & (df_subset[, name_variable] != g)), name_variable] <- 0
  return(df_subset)

}
