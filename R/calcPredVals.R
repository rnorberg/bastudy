#' @title calcPredVals
#' @description xxx
#' @details None at this time
#' @aliases fitReferenceModel
#' @author Jung-han Wang and Robert Norberg
#' @export fitReferenceModel
#' @param refMod A model object created by fitReferenceModel
#' @param trtData A data.frame of treatment crash data.
#'   This data.frame should have column names that exactly match those in mod$model.
#'   It should have an additional column indicating before/after.
#' @param BAcol The name of the column indicating before/after. This column should have no missing data and each value should be one of "Before" or "After".
#' @return Returns the treatment data frame with an additional column named "Predicted".
#' @examples
#' data(Reference)
#' ref_mod <- fitReferenceModel(refData = Reference,
#'  depVar = "kabco_0312",
#'  indepVars = c("Max_AADT", "Min_AADT"),
#'  offset="year")

calcPredVals <- function(refMod, trtData, BAcol = "BeforeAfter"){
  ref_names <- names(mod$model)
  ref_names <- gsub('^offset\\((.*)\\)$', '\\1', ref_names)
  missing_names <- setdiff(ref_names, names(trt))
  if(length(missing_names) > 0){
    stop(paste('The following variable names are present in the model object but not in the supplied Treatment data:', missing_names))
  }

  trtData$Predicted <- predict(mod, newdata = trtData)
  return(trtData)
}
