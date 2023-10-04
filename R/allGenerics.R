#' @importFrom BiocGenerics updateObject
#' @importFrom BiocGenerics sort
#' @importFrom BiocGenerics duplicated
#' @importFrom BiocGenerics as.data.frame

#' @name TaxNormGenerics
#' @title TaxNorm package generics
#' @description TaxNorm package generics; see class man pages for associated
#' methods
#' @param x TaxNorm S4 object
#' @param value Replacement value
#' @param ... Included for extendability; not currently used

NULL

#' @rdname TaxNormGenerics
setGeneric("input_data", function(x, ...) standardGeneric("input_data"))

#' @rdname TaxNormGenerics
setGeneric("input_data<-",
           function(x, ..., value) standardGeneric("input_data<-"))


#' @rdname TaxNormGenerics
setGeneric("rawdata", function(x, ...) standardGeneric("rawdata"))

#' @rdname TaxNormGenerics
setGeneric("rawdata<-",
           function(x, ..., value) standardGeneric("rawdata<-"))

#' @rdname TaxNormGenerics
setGeneric("normdata", function(x, ...) standardGeneric("normdata"))

#' @rdname TaxNormGenerics
setGeneric("normdata<-",
           function(x, ..., value) standardGeneric("normdata<-"))
#' @rdname TaxNormGenerics
setGeneric("ecdf", function(x, ...) standardGeneric("ecdf"))

#' @rdname TaxNormGenerics
setGeneric("ecdf<-",
           function(x, ..., value) standardGeneric("ecdf<-"))
#' @rdname TaxNormGenerics
setGeneric("model_pars", function(x, ...) standardGeneric("model_pars"))

#' @rdname TaxNormGenerics
setGeneric("model_pars<-",
           function(x, ..., value) standardGeneric("model_pars<-"))

#' @rdname TaxNormGenerics
setGeneric("converge", function(x, ...) standardGeneric("converge"))

#' @rdname TaxNormGenerics
setGeneric("converge<-",
           function(x, ..., value) standardGeneric("converge<-"))


#' @rdname TaxNormGenerics
setGeneric("llk", function(x, ...) standardGeneric("llk"))

#' @rdname TaxNormGenerics
setGeneric("llk<-",
           function(x, ..., value) standardGeneric("llk<-"))

#' @rdname TaxNormGenerics
setGeneric("final_df", function(x, ...) standardGeneric("final_df"))

#' @rdname TaxNormGenerics
setGeneric("final_df<-",
           function(x, ..., value) standardGeneric("final_df<-"))

#' @rdname TaxNormGenerics
setGeneric("coefficients", function(x, ...) standardGeneric("coefficients"))

#' @rdname TaxNormGenerics
setGeneric("coefficients<-",
           function(x, ..., value) standardGeneric("coefficients<-"))


#' @rdname TaxNormGenerics
setGeneric("mu", function(x, ...) standardGeneric("mu"))

#' @rdname TaxNormGenerics
setGeneric("mu<-",
           function(x, ..., value) standardGeneric("mu<-"))

#' @rdname TaxNormGenerics
setGeneric("theta", function(x, ...) standardGeneric("theta"))

#' @rdname TaxNormGenerics
setGeneric("theta<-",
           function(x, ..., value) standardGeneric("theta<-"))

#' @rdname TaxNormGenerics
setGeneric("pi", function(x, ...) standardGeneric("pi"))

#' @rdname TaxNormGenerics
setGeneric("pi<-",
           function(x, ..., value) standardGeneric("pi<-"))
