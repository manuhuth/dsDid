#' @title recodeDS
#' @description The function creates a new object of the given vector and replaces certain entries of this vector
#' @param x Vector in which values should be replaced
#' @param replace_with new value
#' @param replace Entry to be replaced
#' @return The recoded vector
#' @export
recodeDS <- function(x, replace_with, replace){

  x <- eval(parse(text=x), envir = parent.frame())

  #form <- eval(parse(text=form), envir = parent.frame())
  x[ x == replace] <- replace_with

  return(x)

}
