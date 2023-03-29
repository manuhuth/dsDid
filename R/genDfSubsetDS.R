#' @title genDfSubsetDS
#' @description The function generates a subset of a dataframe based on a specified variable and value.
#' @param df A dataframe or a string representing a dataframe
#' @param name_variable A character string representing the column in the dataframe to subset on
#' @param value A value or a string representing the value to subset on in the specified column
#' @param include_zero A boolean value indicating whether or not to include rows with a value of zero in the subset
#' @return A subset of the dataframe with rows that match the specified variable and value
#' @export
genDfSubsetDS <- function(df, name_variable, value, include_zero=TRUE){  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  #nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################




  df <- eval(parse(text=df), envir = parent.frame())
  value <- eval(parse(text=value), envir = parent.frame())

  if (isTRUE(include_zero)){
    df <- df[which((df[, name_variable] == value) | (df[, name_variable] == 0) ), ]
  } else{
    df <- df[which((df[, name_variable] == value) ), ]
  }

  if (nrow(df) < nfilter.subset) {
    stop("FAILED: Nvalid less than nfilter.tab", call. = FALSE)
  }


  return(df)

}
