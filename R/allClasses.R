##----------------------------------------------------------------------------##
## All classes
# run this devtools::document() to update NAMESPACE
##----------------------------------------------------------------------------##

#' @import methods
#' @importFrom S4Vectors setValidity2 new2
#' @importClassesFrom S4Vectors character_OR_NULL

NULL

##----------------------------------------------------------------------------##
## TaxaNorm_Model_Parameters

#' @rdname TaxaNorm_Model_Parameters-class
#' @exportClass TaxaNorm_Model_Parameters

setClass("TaxaNorm_Model_Parameters",
         slots = c(coefficients = "matrix",
                   mu = "matrix",
                   theta = "matrix",
                   pi ="matrix"))

setClassUnion("TaxaNorm_Model_Parameters_OR_NULL", members = c("TaxaNorm_Model_Parameters", "NULL"))

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##

## TaxaNorm_Results

#' @rdname TaxaNorm_Results-class
#' @exportClass TaxaNorm_Results

setClass("TaxaNorm_Results",
         slots = c(input_data = "ANY",
                   rawdata = "data.frame",
                   normdata = "data.frame",
                   ecdf = "data.frame",
                   model_pars ="TaxaNorm_Model_Parameters",
                   converge = "logical",
                   llk = "ANY",
                   final_df = "ANY"))

setClassUnion("TaxaNorm_Results_OR_NULL", members = c("TaxaNorm_Results", "NULL"))

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
