#' @title clusterInfluenceFunctionDS
#' @description The function calculates the mean influence for each cluster in a dataframe and returns the values.
#' @param df A string representing a dataframe
#' @param influence_matrix A matrix of influence values
#' @param clustervars A character string or vector of strings representing the column(s) in the dataframe that define the clusters
#' @param idname A character string representing the column in the dataframe that contains the unique identifier for each observation
#' @return A vector of mean influence values for each cluster
#' @export
clusterInfluenceFunctionDS <- function(df, influence_matrix, clustervars, idname){
  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################

  df <- as.matrix(eval(parse(text=df), envir = parent.frame()))


  influence_matrix <- as.matrix(eval(parse(text=influence_matrix), envir = parent.frame()))


  if (nrow(df) < 5) {
    stop("FAILED: Nvalid less than nfilter.tab")
  }

  number_of_clusters <- length(unique(df[, clustervars]))
  cluster <- unlist(unique(df[, c(idname, clustervars)])[,2])
  cluster_n <- aggregate(cluster, by=list(cluster), length)[,2]


  cluster_means <- rowsum(influence_matrix, unlist(cluster), reorder=TRUE) / cluster_n

  return(cluster_means)
}
