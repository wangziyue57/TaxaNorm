#' @importFrom BiocGenerics updateObject
#' @importFrom BiocGenerics sort
#' @importFrom BiocGenerics duplicated
#' @importFrom BiocGenerics as.data.frame

#' @name TaxaNormGenerics
#' @title TaxaNorm package generics
#' @description TaxaNorm package generics; see class man pages for associated
#' methods
#' @param x TaxaNorm S4 object
#' @param value Replacement value
#' @param ... Included for extendability; not currently used

NULL

#' @rdname TaxaNormGenerics
setGeneric("input_data", function(x, ...) standardGeneric("input_data"))

#' @rdname TaxaNormGenerics
setGeneric("input_data<-",
           function(x, ..., value) standardGeneric("input_data<-"))


#' @rdname TaxaNormGenerics
setGeneric("rawdata", function(x, ...) standardGeneric("rawdata"))

#' @rdname TaxaNormGenerics
setGeneric("rawdata<-",
           function(x, ..., value) standardGeneric("rawdata<-"))

#' @rdname TaxaNormGenerics
setGeneric("normdata", function(x, ...) standardGeneric("normdata"))

#' @rdname TaxaNormGenerics
setGeneric("normdata<-",
           function(x, ..., value) standardGeneric("normdata<-"))
#' @rdname TaxaNormGenerics
setGeneric("ecdf", function(x, ...) standardGeneric("ecdf"))

#' @rdname TaxaNormGenerics
setGeneric("ecdf<-",
           function(x, ..., value) standardGeneric("ecdf<-"))
#' @rdname TaxaNormGenerics
setGeneric("model_pars", function(x, ...) standardGeneric("model_pars"))

#' @rdname TaxaNormGenerics
setGeneric("model_pars<-",
           function(x, ..., value) standardGeneric("model_pars<-"))

#' @rdname TaxaNormGenerics
setGeneric("converge", function(x, ...) standardGeneric("converge"))

#' @rdname TaxaNormGenerics
setGeneric("converge<-",
           function(x, ..., value) standardGeneric("converge<-"))


#' @rdname TaxaNormGenerics
setGeneric("llk", function(x, ...) standardGeneric("llk"))

#' @rdname TaxaNormGenerics
setGeneric("llk<-",
           function(x, ..., value) standardGeneric("llk<-"))

#' @rdname TaxaNormGenerics
setGeneric("final_df", function(x, ...) standardGeneric("final_df"))

#' @rdname TaxaNormGenerics
setGeneric("final_df<-",
           function(x, ..., value) standardGeneric("final_df<-"))

#' @rdname TaxaNormGenerics
setGeneric("coefficients", function(x, ...) standardGeneric("coefficients"))

#' @rdname TaxaNormGenerics
setGeneric("coefficients<-",
           function(x, ..., value) standardGeneric("coefficients<-"))


#' @rdname TaxaNormGenerics
setGeneric("mu", function(x, ...) standardGeneric("mu"))

#' @rdname TaxaNormGenerics
setGeneric("mu<-",
           function(x, ..., value) standardGeneric("mu<-"))

#' @rdname TaxaNormGenerics
setGeneric("theta", function(x, ...) standardGeneric("theta"))

#' @rdname TaxaNormGenerics
setGeneric("theta<-",
           function(x, ..., value) standardGeneric("theta<-"))

#' @rdname TaxaNormGenerics
setGeneric("pi", function(x, ...) standardGeneric("pi"))

#' @rdname TaxaNormGenerics
setGeneric("pi<-",
           function(x, ..., value) standardGeneric("pi<-"))
