#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
computeMatrixCrossproductDS <- function(X){
  #matrix comes as string, vector as numbers

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################

  X <- as.matrix(eval(parse(text=X), envir = parent.frame()))

  if (length(unique(as.vector(as.matrix(X)))) < nfilter.tab ){
    stop("FAILED: Nvalid less than nfilter.tab")
  }

  return(t(X) %*% X)

}
