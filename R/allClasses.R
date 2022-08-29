##----------------------------------------------------------------------------##
## All classes
##----------------------------------------------------------------------------##

#' @import methods
#' @importFrom S4Vectors setValidity2 new2
#' @importClassesFrom S4Vectors character_OR_NULL

NULL

##----------------------------------------------------------------------------##
## TaxNorm_Model_Parameters

#' @rdname TaxNorm_Model_Parameters-class
#' @exportClass TaxNorm_Model_Parameters

setClass("TaxNorm_Model_Parameters",
         slots = c(coefficients = "numeric",
                   mu = "numeric",
                   theta = "numeric",
                   pi ="numeric"))

setClassUnion("TaxNorm_Model_Parameters_OR_NULL", members = c("TaxNorm_Model_Parameters", "NULL"))

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##

## TaxNorm_Results

#' @rdname TaxNorm_Results-class
#' @exportClass TaxNorm_Results

setClass("TaxNorm_Results",
         slots = c(rawdata = "data.frame",
                   normdata = "data.frame",
                   ecdf = "data.frame",
                   model_pars ="TaxNorm_Model_Parameters",
                   converge = "logical"))

setClassUnion("TaxNorm_Results_OR_NULL", members = c("TaxNorm_Results", "NULL"))

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
