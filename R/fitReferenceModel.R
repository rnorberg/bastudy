
#' @title fitReferenceModel
#' @description Fits a negative binomial model to the reference data
#' @details None at this time
#' @aliases fitReferenceModel
#' @author Jung-han Wang and Robert Norberg
#' @export fitReferenceModel
#' @param refData A data.frame of reference crash data
#' @param depVar The dependent variable (the number of crashes - should always be of class integer or numeric).
#' @param indepVars Variables used to model the outcome variable depVar
#' @param offset An offset variable (eg years)
#' @return Returns a negative binomial model object
#' @examples
#' data(Reference)
#' fitReferenceModel(refData = Reference,
#'  depVar = "kabco_0312",
#'  indepVars = c("Max_AADT", "Min_AADT"),
#'  offset="year")

fitReferenceModel <- function(refData, depVar, indepVars, offset = NULL){
  form <- paste0(depVar, '~', paste(indepVars, collapse='+'))
  if(!is.null(offset)) form <- paste0(form, ' + offset(', offset, ')')
  init_mod <- MASS::glm.nb(formula = as.formula(form), data = refData)
  step_mod <- MASS::stepAIC(init_mod, trace = FALSE)
  return(step_mod)
}
