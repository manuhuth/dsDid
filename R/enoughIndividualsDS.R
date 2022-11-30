#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param df
#' @param colname
#' @param value
#'
#' @return estimate/prediction using regression coefficients
#' @export
enoughIndividualsDS <- function(df, colname, value){
  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
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
