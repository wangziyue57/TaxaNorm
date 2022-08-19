#' Function to run TaxNorm algorithm
#' @name TaxNorm_Normalization
#' @param data (Required) Input data; should be either a phyloseq object or a count matrix
#' @param depth sequencing depth if pre-calculated. It should be a vector with the same length and order as the column of the count data
#' @param group condition variables if samples are from multiple groups; should be correpsond to the column of the count data. default is NULL, where no grouping is considered
#' @param filter.sample.num,filter.taxa.count taxa with "filter.taxa.count" in more than "filter.sample.num" samples will be removed before testing. default is keep taxa appear in at least 10 samples within each group
#' @param random calculate randomized normal quantile residual
#' @param ncores whether multiple cores is used for parallel computing; default is max(1, detectCores() - 1)
#'
#' @return a list containing the normalized count values
#'
#' @examples
#' TaxNorm_Normalization()
#'
#' @export
