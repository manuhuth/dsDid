#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
clusterInfluenceFunctionDS <- function(df, influence_matrix, clustervars, idname){
  #matrix comes as string, vector as numbers
  if (is.character(df)){
    df <- as.matrix(eval(parse(text=df), envir = parent.frame()))
  }

  if (is.character(influence_matrix)){
    influence_matrix <- as.matrix(eval(parse(text=influence_matrix), envir = parent.frame()))
  }

  number_of_clusters <- length(unique(df[, clustervars]))
  cluster <- unlist(unique(df[, c(idname, clustervars)])[,2])
  cluster_n <- aggregate(cluster, by=list(cluster), length)[,2]
  cluster_means <- rowsum(influence_matrix, unlist(cluster), reorder=TRUE) / cluster_n

  return(cluster_means)
}
