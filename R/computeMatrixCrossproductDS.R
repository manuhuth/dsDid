#' @title computeMatrixCrossproductDS
#' @description The function computes the cross-product of a matrix and returns the result.
#' @param X A string representing a matrix
#' @return A matrix representing the cross-product of the input matrix
#' @export
computeMatrixCrossproductDS <- function(X) {
  # matrix comes as string, vector as numbers

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  # nfilter.glm <- as.numeric(thr$nfilter.glm)
  # nfilter.subset <- as.numeric(thr$nfilter.subset)
  # nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################

  X <- as.matrix(eval(parse(text = X), envir = parent.frame()))

  if (length(unique(as.vector(as.matrix(X)))) < nfilter.tab) {
    # stop("FAILED: Nvalid less than nfilter.tab")
  }

  return(t(X) %*% X)
}
