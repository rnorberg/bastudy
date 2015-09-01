#' @title crossSection
#' @description Calculates Crash Modification Factor (CMF) for a cross-sectional traffic study.
#' @details None at this time
#' @aliases crossSection
#' @author Jung-han Wang and Robert Norberg
#' @export crossSection
#' @param reference Reference data
#' @param depVar The dependent variable (the number of crashes - should always be of class integer or numeric).
#' @param indepVars Variables used to model the outcome variable depVar
#' @param offsetVar An offset variable (eg years)
#' @param forceKeep A character vector of variable names.
#'  These variables will not be considered for removal during the variable selection process.
#' @param alpha Level of confidence
#' @return Returns a list object containing the CMF, its variance, standard error, 1-alpha/2 CI, and negative binomial model
#' @examples
#' data(Reference)
#' empBayes(reference = Reference, depVar = "kabco", offsetVar = "year",target="LTL")

crossSection<-function(reference,depVar, offsetVar = NULL,
                       indepVars =setdiff(names(data),
                                          c(depVar, offsetVar)),
                       forceKeep = NULL,target=NULL,
                       alpha=0.95){
  # check data compatibility
  stopifnot(is.data.frame(reference))

  stopifnot(depVar %in% names(reference))

  stopifnot(all(forceKeep %in% indepVars))

  stopifnot(all(indepVars %in% names(reference)))

  if(!is.null(offsetVar)){
    stopifnot(offsetVar %in% names(reference))
    }

  stopifnot(is.numeric(reference[, depVar]))

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

  init_mod <- MASS::glm.nb(formula = as.formula(full_form), data =
                             data)

  # variable selection using stepwise
  step_mod <- MASS::stepAIC(init_mod, scope = scope, trace = FALSE,
                            direction='both')





  # Get the spot for target variable
  coefSpot<-which(names(step_mod$coefficients)==target)
  targetCoef<-step_mod$coefficients[coefSpot]

  # Locate Standard Error
  stdErr<-summary(step_mod)$coef[,2]
  stdSpot<-which(names(stdErr)==target)
  targetStdErr<-stdErr[stdSpot]

  #Calculate mean CMF
  cmf<-exp(targetCoef)

  #Calculate standard error of CMF
  cmf_se<-(exp(targetCoef+targetStdErr)-exp(targetCoef-targetStdErr))/2

  if (alpha>0.5) {
    alpha<-(1-alpha)/2
    z<-qnorm(alpha)*-1
  }else {
    alpha<-alpha/2
    z<-qnorm(alpha)*-1
  }

  z_int <- z*cmf_se

  cmf_lower <- cmf-z_int
  cmf_upper <- cmf+z_int

  if (cmf_lower <0 ){
    cmf_lower=0
  }


  pval<-summary(step_mod)$coef[,4]
  pvalSpot<-which(names(pval)==target)
  targetPval<-pval[pvalSpot]

  if (targetPval>0.10){
    significance="Target variable is not significant at 90% level"
  } else{significance="Target variable is significant at 90% level"}

  return(list(
    "n" = nrow(data),
    "targetSig"=significance,
    "cmf" = cmf,
    "cmf_se" = cmf_se,
    "cmf_ci" = c('Lower' = cmf_lower,'Upper' = cmf_upper,
                 'alpha' = alpha),
    "nb" = step_mod
  ))
}
