##----------------------------------------------------------------------------##
## methods-TaxaNorm_Results
##----------------------------------------------------------------------------##
#' @importFrom S4Vectors setValidity2 new2
#' @name TaxaNorm_Results-class
#' @title TaxaNorm Results
#' @description S4 class to store TaxaNorm Results
#'
#' @slot input_data `ANY` phyloseq input data
#' @slot rawdata `data.frame` Data frame of counts to use
#' @slot normdata `data.frame` Normalized Data
#' @slot ecdf `data.frame` ecdf
#' @slot model_pars [TaxaNorm_Model_Parameters] list of model parameters
#' @slot converge  `vector(<logical>)` converge
#' @slot llk  `ANY` llk
#' @slot final_df  `ANY` final_df
#' @param input_data passed to `input_data` slot
#' @param rawdata Passed to `rawdata` slot
#' @param normdata Passed to `normdata` slot
#' @param ecdf Passed to `ecdf` slot
#' @param model_pars Passed to `model_pars` slot
#' @param converge Passed to `converge` slot
#' @param llk Passed to `llk` slot
#' @param final_df Passed to `final_df` slot
#' @param x  TaxaNorm_Results object
#' @param value Replacement value

#' @details
#' All results from the TaxaNorm method and what was used to get those results
#' @examples
#' coefficients <-  matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,ncol=3,byrow=TRUE)
#' mu <- matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,ncol=3,byrow=TRUE)
#' theta <-  matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,ncol=3,byrow=TRUE)
#' pi <- matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,ncol=3,byrow=TRUE)
#' model_pars <- TaxaNorm_Model_Parameters(coefficients = coefficients,mu = mu,theta = theta,pi = pi)
#' data("TaxaNorm_Example_Input", package = "TaxaNorm")
#' rawdata <- data.frame(Taxa1 = c(1,2,3),Taxa2 = c(3,4,5),Taxa3 = c(6,7,8))
#' normdata <- data.frame(Taxa1 = c(-1.4,-1.09,-0.73),
#' Taxa2 = c( -0.36,0,0.36), Taxa3 = c(0.73,1.09,1.46))
#' ecdf <- data.frame(0.05,0.23,0.89)
#' converge <- c(TRUE,TRUE,FALSE)
#' llk <- c(1,1.5,0.5)
#' final_df <- data.frame(Taxa1 = c(1,2,3),Taxa2 = c(3,4,5),Taxa3 = c(6,7,8))
#' TaxaNorm_Results(input_data = TaxaNorm_Example_Input,
#'                                   rawdata = rawdata,
#'                                    normdata = normdata,
#'                                    ecdf = ecdf,
#'                                    model_pars = model_pars,
#'                                    converge = converge,
#'                                    llk = llk,
#'                                    final_df = final_df)

NULL

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
## constructor
#' @rdname TaxaNorm_Results-class
#' @export

TaxaNorm_Results <- function(input_data,rawdata, normdata, ecdf, model_pars,converge,llk,final_df) {
  new2("TaxaNorm_Results",
       input_data = input_data,
       rawdata = rawdata,
       normdata = normdata,
       ecdf = ecdf,
       model_pars = model_pars,
       converge = converge,
       llk = llk,
       final_df = final_df)
}

##----------------------------------------------------------------------------##
## accessors


#' @describeIn TaxaNorm_Results-class Return `input_data` slot
#' @aliases TaxaNorm_Results-input_data
#' @export
setMethod("input_data", "TaxaNorm_Results", function(x) {
  x@input_data
})

#' @rdname TaxaNorm_Results-class
#' @export
setReplaceMethod("input_data", "TaxaNorm_Results", function(x,value) {
  x@input_data <- value
  validObject(x)
  x
})

#' @describeIn TaxaNorm_Results-class Return `rawdata` slot
#' @aliases TaxaNorm_Results-rawdata
#' @export
setMethod("rawdata", "TaxaNorm_Results", function(x) {
  x@rawdata
})

#' @rdname TaxaNorm_Results-class
#' @export
setReplaceMethod("rawdata", "TaxaNorm_Results", function(x,value) {
  x@rawdata <- value
  validObject(x)
  x
})

#' @describeIn TaxaNorm_Results-class Return `normdata` slot
#' @aliases TaxaNorm_Results-normdata
#' @export

setMethod("normdata", "TaxaNorm_Results", function(x) {
  x@normdata
})

#' @rdname TaxaNorm_Results-class
#' @export
setReplaceMethod("normdata", "TaxaNorm_Results", function(x,value) {
  x@normdata <- value
  validObject(x)
  x
})

#' @describeIn TaxaNorm_Results-class Return `ecdf` slot
#' @aliases TaxaNorm_Results-ecdf
#' @export
setMethod("ecdf", "TaxaNorm_Results", function(x) {
  x@ecdf
})

#' @rdname TaxaNorm_Results-class
#' @export
setReplaceMethod("ecdf", "TaxaNorm_Results", function(x,value) {
  x@ecdf <- value
  validObject(x)
  x
})

#' @describeIn TaxaNorm_Results-class Return `model_pars` slot
#' @aliases TaxaNorm_Results-model_pars
#' @export
setMethod("model_pars", "TaxaNorm_Results", function(x) {
  x@model_pars
})

#' @rdname TaxaNorm_Results-class
#' @export
setReplaceMethod("model_pars", "TaxaNorm_Results", function(x,value) {
  x@model_pars <- value
  validObject(x)
  x
})


#' @describeIn TaxaNorm_Results-class Return `converge` slot
#' @aliases TaxaNorm_Results-converge
#' @export
setMethod("converge", "TaxaNorm_Results", function(x) {
  x@converge
})

#' @rdname TaxaNorm_Results-class
#' @export
setReplaceMethod("converge", "TaxaNorm_Results", function(x,value) {
  x@converge <- value
  validObject(x)
  x
})


#' @describeIn TaxaNorm_Results-class Return `llk` slot
#' @aliases TaxaNorm_Results-llk
#' @export
setMethod("llk", "TaxaNorm_Results", function(x) {
  x@llk
})

#' @rdname TaxaNorm_Results-class
#' @export
setReplaceMethod("llk", "TaxaNorm_Results", function(x,value) {
  x@llk <- value
  validObject(x)
  x
})

#' @describeIn TaxaNorm_Results-class Return `final_df` slot
#' @aliases TaxaNorm_Results-final_df
#' @export
setMethod("final_df", "TaxaNorm_Results", function(x) {
  x@final_df
})

#' @rdname TaxaNorm_Results-class
#' @export
setReplaceMethod("final_df", "TaxaNorm_Results", function(x,value) {
  x@final_df <- value
  validObject(x)
  x
})

##----------------------------------------------------------------------------##
