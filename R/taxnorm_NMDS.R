#' Function for TaxNorm NMDS
#' @name TaxNorm_NMDS
#' @param TaxNormResults (Required) Input data; should be either a phyloseq object or a count matrix
#' @param group_column column to cluster on
#' @return NMDS Plot
#'
#' @importFrom vegan vegdist metaMDS
#' @importFrom ggplot2 ggplot stat_ellipse aes_string theme_classic
#' @examples
#' \dontrun{TaxNorm_NMDS(TaxNorm_Example_Output, group_column = "body_site")}
#'
#' @export

TaxNorm_NMDS <- function(TaxNormResults,group_column){

  mynorm <- normdata(TaxNormResults)

  mydata <- input_data(TaxNormResults)

  normdata <- mynorm + abs(min(mynorm, na.rm = T))

  dist <- vegan::vegdist(t(normdata), method="bray", na.rm = T)
  fit <- vegan::metaMDS(dist, k=2)

  norm_NMDS <- data.frame(fit$points, sample_data(mydata))

 p <- ggplot(norm_NMDS) +
    geom_point(aes_string('MDS1', 'MDS2', color = group_column)) +
    stat_ellipse(aes_string('MDS1', 'MDS2', color = group_column)) +
    theme_classic()

  return(p)

}
