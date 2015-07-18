#' @title calcPredVals
#' @description xxx
#' @details None at this time
#' @aliases fitReferenceModel
#' @author Jung-han Wang and Robert Norberg
#' @export fitReferenceModel
#' @param trtData Treatment data returned from calling calcPredVals()
#' @param refModel Reference model returned from calling fitReferencModel()
#' @param BAcol The name of the column indicating before/after. This column should have no missing data and each value should be one of "Before" or "After".
#' @param alpha Level of confidence
#' @return Returns the treatment data frame with an additional column named "Predicted".
#' @examples
#' ref_mod <- fitReferenceModel(refData = Reference,
#'  depVar = "kabco_0312",
#'  indepVars = c("Max_AADT", "Min_AADT"),
#'  offset="year")

empBayes <- function(trtData, refModel, BAcol = 'BeforeAfter', alpha = 0.95){
  # get dependent variable from model object
  depVar <- grep('~|\\+', as.character(refMod$terms), invert = TRUE, value = TRUE)

  # treatment # crashes before
  tb <- sum(trtData[grepl('Before', trtData[, BAcol], ignore.case=T), depVar])
  # treatment # crashes after
  ta <- sum(trtData[grepl('After', trtData[, BAcol], ignore.case=T), depVar])
  # predicted # crashes before (predicted for Treatment from model fit to Reference)
  pb <- sum(trtData[grepl('Before', trtData[, BAcol], ignore.case=T), 'Predicted'])
  # predicted # crashes after (predicted for Treatment from model fit to Reference)
  pa <- sum(trtData[grepl('After', trtData[, BAcol], ignore.case=T), 'Predicted'])

  # Overdispersion parameter
  k <- refMod$theta

  weight=1/(1+pb*(1/k))

  N_exp_tb<-weight*pb+(1-weight)*tb
  eb_ratio<-pa/pb

  N_exp_ta<-N_exp_tb*(eb_ratio)
  var_N_exp_ta<-N_exp_ta*(eb_ratio)*(1-weight)

  lamda<-ta
  cmf<-(lamda/N_exp_ta)/(1+var_N_exp_ta/var_N_exp_ta**2)
  cmf_var<-cmf**2*((1/N_exp_ta)+(var_N_exp_ta/N_exp_ta**2))/(1+var_N_exp_ta/N_exp_ta**2)**2

  ## Calculate Standard Error of Crash Reduction Index
  cmf_se<-sqrt(cmf_var)

  if (alpha>0.5) {
    alpha<-(1-alpha)/2
    z<-qnorm(alpha)*-1
  }else {
    alpha<-alpha/2
    z<-qnorm(alpha)*-1
  }

  z_int<-z*sqrt(cmf_var)

  cmf_lower<-cmf-z_int
  cmf_upper<-cmf+z_int

  if (cmf_lower < 0) {
    cmf_lower = 0
  }

  return(cat('\n',"Sample Size =",nrow(trtData)/2,'\n',
             "CMF =",cmf,'\n',"CMF Variance =",cmf_var,'\n',"CMF Standard Error =",cmf_se,'\n',"CMF Lower Bound =",cmf_lower,'\n',"CMF Upper Bound =",cmf_upper))
}
