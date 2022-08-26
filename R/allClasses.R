##----------------------------------------------------------------------------##
## All classes
##----------------------------------------------------------------------------##

#' @import methods
#' @importFrom S4Vectors setValidity2 new2
#' @importClassesFrom S4Vectors character_OR_NULL

NULL

##----------------------------------------------------------------------------##
## Virtual classes

#' @importClassesFrom S4Vectors SimpleList
#' @importClassesFrom S4Vectors List
#' @importFrom S4Vectors List

setClass("NamedList", contains = c("VIRTUAL", "SimpleList"))

##----------------------------------------------------------------------------##
## TaxNorm_Model_Parameters

#' @rdname TaxNorm_Model_Parameters-class
#' @exportClass TaxNorm_Model_Parameters

setClass("TaxNorm_Model_Parameters",
         slots = c(coefficients = "vector(<numeric>)",
                   mu = "numeric",
                   theta = "numeric",
                   pi ="numeric"))

setClassUnion("TaxNorm_Model_Parameters_OR_NULL", members = c("TaxNorm_Model_Parameters", "NULL"))

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
