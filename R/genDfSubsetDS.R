#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
genDfSubsetDS <- function(df, name_variable, value, include_zero=TRUE){  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
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
