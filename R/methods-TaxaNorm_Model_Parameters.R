
## methods-TaxaNorm_Model_Parameters
##----------------------------------------------------------------------------##

#' @name TaxaNorm_Model_Parameters-class
#' @title TaxaNorm_Model_Parameters
#' @description S4 class to store TaxaNorm Parameters
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
#' @param x  TaxaNorm_Model_Parameters object
#' @param value Replacement value

#' @details
#' Parameters for TaxaNorm Method
#' @examples \dontrun{TaxaNorm_Model_Parameters(coefficients = coefficients,mu = mu,theta = theta,pi = pi)}

NULL

##----------------------------------------------------------------------------##
## constructor
#' @rdname TaxaNorm_Model_Parameters-class
#' @export

TaxaNorm_Model_Parameters <- function(coefficients,mu,theta,pi) {
  new2("TaxaNorm_Model_Parameters",
       coefficients = coefficients,
       mu = mu,
       theta = theta,
       pi = pi)
}

##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## accessors

#' @describeIn TaxaNorm_Model_Parameters-class Return `coefficients` slot
#' @aliases TaxaNorm_Model_Parameters-coefficients
#' @export

setMethod("coefficients", "TaxaNorm_Model_Parameters", function(x) {
  x@coefficients
})

#' @rdname TaxaNorm_Model_Parameters-class
#' @export
setReplaceMethod("coefficients", "TaxaNorm_Model_Parameters", function(x,value) {
  x@coefficients <- value
  validObject(x)
  x
})

#' @describeIn TaxaNorm_Model_Parameters-class Return `mu` slot
#' @aliases TaxaNorm_Model_Parameters-mu
#' @export

setMethod("mu", "TaxaNorm_Model_Parameters", function(x) {
  x@mu
})

#' @rdname TaxaNorm_Model_Parameters-class
#' @export
setReplaceMethod("mu", "TaxaNorm_Model_Parameters", function(x,value) {
  x@mu <- value
  validObject(x)
  x
})

#' @describeIn TaxaNorm_Model_Parameters-class Return `theta` slot
#' @aliases TaxaNorm_Model_Parameters-theta
#' @export

setMethod("theta", "TaxaNorm_Model_Parameters", function(x) {
  x@theta
})

#' @rdname TaxaNorm_Model_Parameters-class
#' @export
setReplaceMethod("theta", "TaxaNorm_Model_Parameters", function(x,value) {
  x@theta <- value
  validObject(x)
  x
})
#' @describeIn TaxaNorm_Model_Parameters-class Return `pi` slot
#' @aliases TaxaNorm_Model_Parameters-pi
#' @export

setMethod("pi", "TaxaNorm_Model_Parameters", function(x) {
  x@pi
})

#' @rdname TaxaNorm_Model_Parameters-class
#' @export
setReplaceMethod("pi", "TaxaNorm_Model_Parameters", function(x,value) {
  x@pi <- value
  validObject(x)
  x
})

##----------------------------------------------------------------------------##

