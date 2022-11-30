#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
AppendInfluenceDS <- function(seed, df, influences, id_period_vector, column){



  #matrix comes as string, vector as numbers
  if (is.character(df)){
    df <- eval(parse(text=df), envir = parent.frame())
  }

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################


  if (nrow(df) < nfilter.tab) {
    stop("FAILED: Nvalid less than nfilter.tab")
  }


  if (is.character(id_period_vector)){
    id_period_vector <- eval(parse(text=id_period_vector), envir = parent.frame())
  }

  if (is.character(column)){
    column <- eval(parse(text=column), envir = parent.frame())
  }

  df[which( rownames(df) %in% id_period_vector), column] <- influences

  return(df[sample(nrow(df)),])
}
