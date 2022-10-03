#' Function to QC TaxNorm algorithm
#' @name TaxNorm_Model_QC
#' @param data (Required) Input data; should be either a phyloseq object or a count matrix
#' @param group condition variables if samples are from multiple groups; should be correpsond to the column of the count data. default is NULL, where no grouping is considered
#' @param filter.sample.num,filter.taxa.count taxa with "filter.taxa.count" in more than "filter.sample.num" samples will be removed before testing. default is keep taxa appear in at least 10 samples within each group
#'
#' @return a list containing qc taxnorm object
#'
#' @examples
#' TaxNorm_Model_QC()
#'
#' @export


TaxNorm_Model_QC <- function(TaxNorm_Results){

  mymodelpars <- model_pars(TaxNorm_Example_Output)



}
