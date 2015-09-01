#' @title CompGroup
#' @description Calculates Crash Modification Factor (CMF) for a before/after traffic study using Comparison Method
#' @details None at this time
#' @aliases CompGroup
#' @author Jung-han Wang and Robert Norberg
#' @export CompGroup
#' @param compBefore Comparison data in before period
#' @param compAfter Comparison data in after period
#' @param before Treatment data, before some change was made
#' @param after Treatment data, after some change was made
#' @param depVar The dependent variable (the number of crashes - should always be of class integer or numeric).
#' @param alpha Level of confidence
#' @return Returns a list object containing the CMF, its variance, standard error, and 1-alpha/2 CI
#' @examples
#' data(compBefore)
#' data(compAfter)
#' data(Before)
#' data(After)
#' CompGroup(compBefore = compBefore, compAfter = compAfter, before = Before, after = After,
#'  depVar = "kabco")

CompGroup<-function(compBefore,compAfter, before, after, depVar, alpha = 0.95){
  # check data compatibility
  stopifnot(is.data.frame(compBefore))
  stopifnot(is.data.frame(compAfter))
  stopifnot(is.data.frame(before))
  stopifnot(is.data.frame(after))

  stopifnot(nrow(before) == nrow(after))

  stopifnot(depVar %in% names(compBefore))
  stopifnot(depVar %in% names(compAfter))
  stopifnot(depVar %in% names(before))
  stopifnot(depVar %in% names(after))

  # treatment # crashes before
  tb <- sum(before[, depVar])
  # treatment # crashes after
  ta <- sum(after[, depVar])
  # Comparison # crashes before
  cb <- sum(compBefore[, depVar])
  # Comparison # crashes after
  ca <- sum(compAfter[, depVar])

  tb<-sum(tb)
  ta<-sum(ta)
  cb<-sum(cb)
  ca<-sum(ca)

  sample_odd_ratio<-((tb*ca)/(ta*cb))/(1+(1/90)+(1/95))
  comparison_ratio<-ca/cb
  N_exp_ta<-tb*comparison_ratio

  ##Mohamed's Standard Error
  var_N_exp_ta<-N_exp_ta**2*((1/tb)/((cb+1)/ca))

  ## CMF Guide and Dr. Ezra Hauer
  #var_N_exp_ta<-N_exp_ta**2*(1/tb+1/cb+1/ca)

  lamda<-sum(ta)
  cmf<-(lamda/N_exp_ta)/(1+var_N_exp_ta/var_N_exp_ta**2)
  cmf_var<-cmf**2*((1/N_exp_ta)+(var_N_exp_ta/N_exp_ta**2))/(1+var_N_exp_ta/N_exp_ta**2)**2

  ## Calculate Standard Error of Crash Reduction Index
  cmf_se<-sqrt(cmf_var)

  if (alpha>0.5) {
    alpha<-(1-alpha)/2
    z<-qnorm(alpha)*-1
  }
  else {
    alpha<-alpha/2
    z<-qnorm(alpha)*-1
  }

  z_int<-z*sqrt(cmf_var)

  cmf_lower<-cmf-z_int
  cmf_upper<-cmf+z_int

  if (cmf_lower < 0) {
    cmf_lower = 0
  }

  return(list(
    "n" = nrow(before),
    "cmf" = cmf,
    "cmf_variance" = cmf_var,
    "cmf_se" = cmf_se,
    "cmf_ci" = c('Lower' = cmf_lower,'Upper' = cmf_upper, 'alpha' = alpha)
  ))
}

