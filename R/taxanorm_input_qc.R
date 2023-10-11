#' Function for TaxNorm input data
#' @name TaxaNorm_QC_Input
#' @param data (Required) Input data; should be either a phyloseq object or a count matrix
#'
#' @return QC PLots
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth scale_x_continuous scale_y_continuous geom_abline ggtitle theme_classic
#' @examples
#' \dontrun{qc_data <- TaxaNorm_QC_Input(TaxaNorm_Example_Input)}
#'
#' @export



TaxaNorm_QC_Input <- function(data){

  var <- NULL
  zero <- NULL

  depth <- sample_sums(data)

  countdata <- abundances(data)
  mean_all <- rowMeans(countdata)
  zero_all <- rowSums(otu_table(data)==0)
  var_all <- matrixStats::rowVars(countdata) # taxa varaince

  data_summary <- data.frame(mean = mean_all, var = var_all, zero = zero_all)
  # inspect mean-variance relationship
  # fit a local regression line (loess)
  # parameter: span=1 can be changed. larger value will give less overfitted line
  p <- ggplot(data_summary, aes(mean, var)) +
    geom_point() +
    geom_smooth(method = "loess", span = 1, method.args = list(degree=2), color = "red") +
    scale_x_continuous(trans = 'log10') +
    scale_y_continuous(trans = 'log10') +
    geom_abline(intercept = 0, slope = 1, linetype = 1, size = 0.5,
                color = "gray60") +
    ggtitle("Mean Variance Relationship") +
    theme_classic()

  print(p)

  # inspect mean-zero relationship
  g <- ggplot(data_summary, aes(mean, zero)) +
    geom_point() +
    geom_smooth(method = "loess", span = 1, method.args = list(degree=1), color = "red") +
    scale_x_continuous(trans = 'log10') +
    scale_y_continuous(trans = 'log10') +
    geom_abline(intercept = 0, slope = 1, linetype = 1, size = 0.5,
                color = "gray60") +
    ggtitle("Mean Zero Relationship") +
    theme_classic()
print(g)

  return(data_summary)

}
