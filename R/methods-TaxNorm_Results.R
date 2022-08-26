##----------------------------------------------------------------------------##
## methods-TaxNorm_Results
##----------------------------------------------------------------------------##

#' @name TaxNorm_Results-class
#' @title TaxNorm Results
#' @description S4 class to store TaxNorm Results
#'
#' @slot rawdata `data.frame` Data frame of counts to use
#' @slot normdata `data.frame` Normalized Data
#' @slot ecdf `data.frame` ecdf
#' @slot model_pars [TaxNorm_Model_Parameters] list of model parameters
#' @slot converge  `vector(<logical>)`
#'
#' @param rawdata Passed to `rawdata` slot
#' @param normdata Passed to `normdata` slot
#' @param ecdf Passed to `ecdf` slot
#' @param model_pars Passed to `model_pars` slot
#' @param converge Passed to `converge` slot
#'
#' @details
#' All results from the TaxNorm method and what was used to get those results
#' @examples

NULL

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
## constructor
#' @rdname TaxNorm_Results-class
#' @export

TaxNorm_Results <- function(rawdata, normdata, ecdf, model_pars,converge) {
  new2("TaxNorm_Results",
       rawdata = rawdata,
       normdata = normdata,
       ecdf = ecdf,
       model_pars = model_pars,
       converge = converge)
}

##----------------------------------------------------------------------------##
