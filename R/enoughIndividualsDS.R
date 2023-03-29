#' @title enoughIndividualsDS
#' @description The function checks if there are enough individuals with a specified value in a specified column of a dataframe.
#' @param df A dataframe or a string representing a dataframe
#' @param colname A character string representing the column in the dataframe to check for the specified value
#' @param value A value or a string representing the value to check for in the specified column
#' @return A boolean indicating whether or not there are enough individuals with the specified value in the specified column.
#' @export
enoughIndividualsDS <- function(df, colname, value){
  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  #nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################


  #matrix comes as string, vector as numbers
  if (is.character(df)){
    df <- eval(parse(text=df), envir = parent.frame())
  }

  if (is.character(value)){
    value <- as.numeric(eval(parse(text=value), envir = parent.frame()))
  }

  boolean_statement <- length(df[which(df[ , colname] == value), colname]) >= nfilter.subset

  return(boolean_statement)
}
