##----------------------------------------------------------------------------##
## methods-TaxNorm_Results
##----------------------------------------------------------------------------##
#' @importFrom S4Vectors setValidity2 new2
#' @name TaxNorm_Results-class
#' @title TaxNorm Results
#' @description S4 class to store TaxNorm Results
#'
#' @slot input_data `ANY` phyloseq input data
#' @slot rawdata `data.frame` Data frame of counts to use
#' @slot normdata `data.frame` Normalized Data
#' @slot ecdf `data.frame` ecdf
#' @slot model_pars [TaxNorm_Model_Parameters] list of model parameters
#' @slot converge  `vector(<logical>)`
#'
#' @param input_data passed to `input_data` slot
#' @param rawdata Passed to `rawdata` slot
#' @param normdata Passed to `normdata` slot
#' @param ecdf Passed to `ecdf` slot
#' @param model_pars Passed to `model_pars` slot
#' @param converge Passed to `converge` slot
#' @param x  TaxNorm_Results object
#' @param value Replacement value

#' @details
#' All results from the TaxNorm method and what was used to get those results
#' @examples

NULL

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
## constructor
#' @rdname TaxNorm_Results-class
#' @export

TaxNorm_Results <- function(input_data,rawdata, normdata, ecdf, model_pars,converge) {
  new2("TaxNorm_Results",
       input_data = input_data,
       rawdata = rawdata,
       normdata = normdata,
       ecdf = ecdf,
       model_pars = model_pars,
       converge = converge)
}

##----------------------------------------------------------------------------##
## accessors


#' @describeIn TaxNorm_Results-class Return `input_data` slot
#' @aliases TaxNorm_Results-input_data
#' @export

setMethod("input_data", "TaxNorm_Results", function(x) {
  x@input_data
})

#' @rdname TaxNorm_Results-class
#' @export
setReplaceMethod("input_data", "TaxNorm_Results", function(x,value) {
  x@input_data <- value
  validObject(x)
  x
})

#' @describeIn TaxNorm_Results-class Return `rawdata` slot
#' @aliases TaxNorm_Results-rawdata
#' @export

setMethod("rawdata", "TaxNorm_Results", function(x) {
  x@rawdata
})

#' @rdname TaxNorm_Results-class
#' @export
setReplaceMethod("rawdata", "TaxNorm_Results", function(x,value) {
  x@rawdata <- value
  validObject(x)
  x
})

#' @describeIn TaxNorm_Results-class Return `normdata` slot
#' @aliases TaxNorm_Results-normdata
#' @export

setMethod("normdata", "TaxNorm_Results", function(x) {
  x@normdata
})

#' @rdname TaxNorm_Results-class
#' @export
setReplaceMethod("normdata", "TaxNorm_Results", function(x,value) {
  x@normdata <- value
  validObject(x)
  x
})

#' @describeIn TaxNorm_Results-class Return `ecdf` slot
#' @aliases TaxNorm_Results-ecdf
#' @export

setMethod("ecdf", "TaxNorm_Results", function(x) {
  x@ecdf
})

#' @rdname TaxNorm_Results-class
#' @export
setReplaceMethod("ecdf", "TaxNorm_Results", function(x,value) {
  x@ecdf <- value
  validObject(x)
  x
})

#' @describeIn TaxNorm_Results-class Return `model_pars` slot
#' @aliases TaxNorm_Results-model_pars
#' @export

setMethod("model_pars", "TaxNorm_Results", function(x) {
  x@model_pars
})

#' @rdname TaxNorm_Results-class
#' @export
setReplaceMethod("model_pars", "TaxNorm_Results", function(x,value) {
  x@model_pars <- value
  validObject(x)
  x
})


#' @describeIn TaxNorm_Results-class Return `converge` slot
#' @aliases TaxNorm_Results-converge
#' @export

setMethod("converge", "TaxNorm_Results", function(x) {
  x@converge
})

#' @rdname TaxNorm_Results-class
#' @export
setReplaceMethod("converge", "TaxNorm_Results", function(x,value) {
  x@converge <- value
  validObject(x)
  x
})
##----------------------------------------------------------------------------##
