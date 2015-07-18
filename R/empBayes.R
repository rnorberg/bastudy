#' @title empBayes
#' @description Calculates Crash Modification Factor (CMF) for a before/after traffic study using Empirical Bayes to avoid regression to the mean (RTM)
#' @details None at this time
#' @aliases empBayes
#' @author Jung-han Wang and Robert Norberg
#' @export empBayes
#' @param reference Reference data
#' @param before Treatment data, before some change was made
#' @param after Treatment data, after some change was made
#' @param depVar The dependent variable (the number of crashes - should always be of class integer or numeric).
#' @param indepVars Variables used to model the outcome variable depVar
#' @param offsetVar An offset variable (eg years)
#' @param forceKeep A character vector of variable names.
#'  These variables will not be considered for removal during the variable selection process.
#' @param alpha Level of confidence
#' @return Returns a list object containing the CMF, its variance, standard error, and 1-alpha/2 CI
#' @examples
#' data(Reference)
#' data(Before)
#' data(After)
#' empBayes(reference = Reference, before = Before, after = After,
#'  depVar = "kabco", offsetVar = "year")

empBayes <- function(reference, before, after,
                     depVar, offsetVar = NULL, indepVars = setdiff(names(reference), c(depVar, offsetVar)),
                     forceKeep = NULL,
                     alpha = 0.95){

  # check data compatibility
  stopifnot(is.data.frame(reference))
  stopifnot(is.data.frame(before))
  stopifnot(is.data.frame(after))

  stopifnot(nrow(before) == nrow(after))

  stopifnot(depVar %in% names(reference))
  stopifnot(depVar %in% names(before))
  stopifnot(depVar %in% names(after))

  stopifnot(all(forceKeep %in% indepVars))

  stopifnot(all(indepVars %in% names(reference)))
  stopifnot(all(indepVars %in% names(before)))
  stopifnot(all(indepVars %in% names(after)))

  if(!is.null(offsetVar)){
    stopifnot(offsetVar %in% names(reference))
    stopifnot(offsetVar %in% names(before))
    stopifnot(offsetVar %in% names(after))
  }

  stopifnot(is.numeric(reference[, depVar]))
  stopifnot(is.numeric(before[, depVar]))
  stopifnot(is.numeric(after[, depVar]))

  # fit negative binomial model to reference data
  full_form <- paste0(depVar, '~', paste(indepVars, collapse='+'))
  if(!is.null(forceKeep)){
    min_form <- paste0(depVar, '~', paste(forceKeep, collapse='+'))
  }else{
    min_form <- NULL
  }

  if(!is.null(offsetVar)){
    full_form <- paste0(full_form, ' + offset(', offsetVar, ')')
    if(!is.null(forceKeep)){
      min_form <- paste0(min_form, ' + offset(', offsetVar, ')')
    }
  }

  if(!is.null(forceKeep)){
    scope <- list('lower' = as.formula(min_form), 'upper' = as.formula(full_form))
  }else{
    scope <- as.formula(full_form)
  }

  init_mod <- MASS::glm.nb(formula = as.formula(full_form), data = reference)
  # variable selection using stepwise (Dr. Johnson forgive me)
  step_mod <- MASS::stepAIC(init_mod, scope = scope, trace = FALSE, direction='both')

  # use reference model to calculate expected values for before and after data
  before$Expected <- predict(step_mod, newdata = before)
  after$Expected <- predict(step_mod, newdata = after)

  # treatment # crashes before
  tb <- sum(before[, depVar])
  # treatment # crashes after
  ta <- sum(after[, depVar])
  # expected # crashes before (calculated using model fit to Reference)
  pb <- sum(before$Expected)
  # expected # crashes after (calculated using model fit to Reference)
  pa <- sum(after$Expected)

  # overdispersion parameter
  k <- step_mod$theta

  weight <- 1/(1+pb*(1/k))

  N_exp_tb <- weight*pb+(1-weight)*tb
  eb_ratio <- pa/pb

  N_exp_ta <- N_exp_tb*(eb_ratio)
  var_N_exp_ta <- N_exp_ta*(eb_ratio)*(1-weight)

  lamda <- ta
  cmf <- (lamda/N_exp_ta)/(1+var_N_exp_ta/var_N_exp_ta**2)
  cmf_var<-cmf**2*((1/N_exp_ta)+(var_N_exp_ta/N_exp_ta**2))/(1+var_N_exp_ta/N_exp_ta**2)**2

  # calculate standard error of crash reduction index
  cmf_se<-sqrt(cmf_var)

  if (alpha>0.5) {
    alpha<-(1-alpha)/2
    z<-qnorm(alpha)*-1
  }else {
    alpha<-alpha/2
    z<-qnorm(alpha)*-1
  }

  z_int <- z*sqrt(cmf_var)

  cmf_lower <- cmf-z_int
  cmf_upper <- cmf+z_int

  if (cmf_lower < 0) {
    cmf_lower = 0
  }

  return(list(
    "n" = nrow(before)/2,
    "cmf" = cmf,
    "cmf_variance" = cmf_var,
    "cmf_se" = cmf_se,
    "cmf_ci" = c('Lower' = cmf_lower,'Upper' = cmf_upper, 'alpha' = alpha)
    ))
}
