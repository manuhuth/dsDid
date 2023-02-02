#'
#' Computes odds from propensities and a dummy variable
#'
#' @param name_propensities: A vector of propensities represented as a string
#' @param name_dummy_g: A vector of dummy variables representing a binary outcome represented as a string
#' @return: A vector of odds computed from the propensities and the binary outcome
#'
#' @export
computeOddsDS <- function(name_propensities, name_dummy_g){
  #matrix comes as string, vector as numbers

  dummy_G <- eval(parse(text=name_dummy_g), envir = parent.frame())
  propensities <- eval(parse(text=name_propensities), envir = parent.frame())
  C <- 1 - dummy_G
  odds <- C * propensities / (1 - propensities)

  return(odds)

}
