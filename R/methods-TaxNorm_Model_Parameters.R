
## methods-TaxNorm_Model_Parameters
##----------------------------------------------------------------------------##

#' @name TaxNorm_Model_Parameters-class
#' @title TaxNorm_Model_Parameters
#' @description S4 class to store TaxNorm Parameters
#'
#' @slot coefficients `matrix` coefficients
#' @slot mu `matrix` mu
#' @slot theta `matrix` theta
#' @slot pi `matrix` pi
#'
#' @param coefficients Passed to `coefficients` slot
#' @param mu Passed to `mu` slot
#' @param theta Passed to `theta` slot
#' @param pi Passed to `pi` slot
#'
#' @details
#' Parameters for TaxNorm Method
#' @examples

NULL

##----------------------------------------------------------------------------##
## constructor
#' @rdname TaxNorm_Model_Parameters-class
#' @export

TaxNorm_Model_Parameters <- function(coefficients,mu,theta,pi) {
  new2("TaxNorm_Model_Parameters",
       coefficients = coefficients,
       mu = mu,
       theta = theta,
       pi = pi)
}

##----------------------------------------------------------------------------##
