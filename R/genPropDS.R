#' assign function -- provide estimate of found coefficients (typically from federated RDataShield GLM function)
#'
#' @param form formula in text format
#' @param coefficients coefficients of dependent variables in form
#' @param object dataframe object name where dependent var values are located
#' @param invlog use inverse of logit
#'
#' @return estimate/prediction using regression coefficients
#' @export
genPropDS <- function(form, coefficents, object, invlog, constant_in_matrix = FALSE){

  form <- eval(parse(text=form), envir = parent.frame())
  form.vars <- all.vars(formula(form))
  object <- eval(parse(text=object), envir = parent.frame())


  estimate <- object[form.vars[1]]
  estimate[,] <- coefficents[1]
  colnames(estimate) <- "distance"

  if (!constant_in_matrix){
    for (i in 2:length(form.vars)){
      estimate <- estimate + object[form.vars[i]] * coefficents[i]
    }
  } else{
    for (i in 3:length(form.vars)){
      estimate <- estimate + object[form.vars[i]] * coefficents[i-1]
    }
  }


  # inverse logit
  if (invlog){
    estimate <- 1 / (1 + exp(-estimate))
    # estimate <- invlogit(estimate)
  }

  return(as.numeric(estimate[["distance"]]))

}
