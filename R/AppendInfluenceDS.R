#' @title AppendInfluenceDS
#' @description The function appends a column of influence values to a dataframe and returns the new dataframe.
#' @param seed A seed used for random sampling
#' @param df A string representing a dataframe
#' @param influences A vector of influence values
#' @param id_period_vector A vector of row names or a string representing a vector of row names
#' @param column A character string representing the name of the column to append the influence values to
#' @return A dataframe with the column of influence values added
#' @export
AppendInfluenceDS <- function(df, influences, id_period_vector, column){

  df <- eval(parse(text=df), envir = parent.frame())


  influences <- eval(parse(text=influences), envir = parent.frame())


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


  df <- df[as.character(sort(as.numeric(row.names(df)))), ]

  id_period_vector <- sort(id_period_vector)

  df[which( rownames(df) %in% id_period_vector), column] <- influences


  return(df[sample(nrow(df)),])
  #return(df)
}
