#' Function for TaxNorm NMDS
#' @name TaxNorm_NMDS
#' @param TaxNormResults (Required) Input data; should be either a phyloseq object or a count matrix
#'
#' @return NMDS Plot
#'
#' @examples
#' TaxNorm_NMDS()
#'
#' @export

TaxNorm_NMDS <- function(TaxNormResults,group){

  mynorm <- normdata(TaxNormResults)

  mydata <- input_data(TaxNormResults)

  normdata <- mynorm + abs(min(mynorm, na.rm = T))

  dist <- vegan::vegdist(t(normdata), method="bray", na.rm = T)
  fit <- vegan::metaMDS(dist, k=2)

  norm_NMDS <- data.frame(fit$points, sample_data(mydata))

 p <- ggplot(norm_NMDS) +
    geom_point(aes_string('MDS1', 'MDS2', color = group)) +
    stat_ellipse(aes_string('MDS1', 'MDS2', color = group)) +
    theme_classic()

  return(p)

}
