
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
#' @param x  TaxNorm_Model_Parameters object
#' @param value Replacement value

#' @details
#' Parameters for TaxNorm Method
#' @examples \dontrun{
#' TaxNorm_Model_Parameters(coefficients = coefficients,mu = mu,theta = theta,pi = pi)}

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

##----------------------------------------------------------------------------##
## accessors

#' @describeIn TaxNorm_Model_Parameters-class Return `coefficients` slot
#' @aliases TaxNorm_Model_Parameters-coefficients
#' @export

setMethod("coefficients", "TaxNorm_Model_Parameters", function(x) {
  x@coefficients
})

#' @rdname TaxNorm_Model_Parameters-class
#' @export
setReplaceMethod("coefficients", "TaxNorm_Model_Parameters", function(x,value) {
  x@coefficients <- value
  validObject(x)
  x
})

#' @describeIn TaxNorm_Model_Parameters-class Return `mu` slot
#' @aliases TaxNorm_Model_Parameters-mu
#' @export

setMethod("mu", "TaxNorm_Model_Parameters", function(x) {
  x@mu
})

#' @rdname TaxNorm_Model_Parameters-class
#' @export
setReplaceMethod("mu", "TaxNorm_Model_Parameters", function(x,value) {
  x@mu <- value
  validObject(x)
  x
})

#' @describeIn TaxNorm_Model_Parameters-class Return `theta` slot
#' @aliases TaxNorm_Model_Parameters-theta
#' @export

setMethod("theta", "TaxNorm_Model_Parameters", function(x) {
  x@theta
})

#' @rdname TaxNorm_Model_Parameters-class
#' @export
setReplaceMethod("theta", "TaxNorm_Model_Parameters", function(x,value) {
  x@theta <- value
  validObject(x)
  x
})
#' @describeIn TaxNorm_Model_Parameters-class Return `pi` slot
#' @aliases TaxNorm_Model_Parameters-pi
#' @export

setMethod("pi", "TaxNorm_Model_Parameters", function(x) {
  x@pi
})

#' @rdname TaxNorm_Model_Parameters-class
#' @export
setReplaceMethod("pi", "TaxNorm_Model_Parameters", function(x,value) {
  x@pi <- value
  validObject(x)
  x
})

##----------------------------------------------------------------------------##

