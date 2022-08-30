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
setGeneric("rawdata", function(x, ...) standardGeneric("rawdata"))

#' @rdname TaxNormGenerics
setGeneric("rawdata<-", 
           function(x, ..., value) standardGeneric("rawdata<-"))

