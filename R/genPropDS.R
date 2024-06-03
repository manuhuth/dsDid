#' @title genPropDS
#' @description The function calculates the estimate of a variable based on the given formula, coefficients and object, and returns the numeric value of the variable.
#' @param form A formula in character string format.
#' @param coefficients A numeric vector of coefficients for the variables in the formula
#' @param object An R object that contains the variables in the formula
#' @param invlog A logical value indicating whether to apply inverse logit transformation on the estimate
#' @param constant_in_matrix A logical value indicating whether the constant term is included in the coefficient matrix
#' @return A numeric value representing the estimate of the variable
#' @export
genPropDS <- function(form, coefficents, object, invlog, constant_in_matrix = FALSE) {
  # form <- eval(parse(text=form), envir = parent.frame())
  form.vars <- all.vars(formula(form))
  object <- eval(parse(text = object), envir = parent.frame())


  estimate <- object[form.vars[1]]
  estimate[, ] <- coefficents[1]
  colnames(estimate) <- "distance"

  if (!constant_in_matrix) {
    for (i in 2:length(form.vars)) {
      estimate <- estimate + object[form.vars[i]] * coefficents[i]
    }
  } else {
    for (i in 3:length(form.vars)) {
      estimate <- estimate + object[form.vars[i]] * coefficents[i - 1]
    }
  }


  # inverse logit
  if (invlog) {
    estimate <- 1 / (1 + exp(-estimate))
    # estimate <- invlogit(estimate)
  }

  return(as.numeric(estimate[["distance"]]))
}
