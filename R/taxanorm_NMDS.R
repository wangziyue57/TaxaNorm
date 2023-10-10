#' Function for TaxNorm NMDS
#' @name TaxaNorm_NMDS
#' @param TaxaNormResults (Required) Input data; should be either a phyloseq object or a count matrix
#' @param group_column column to cluster on
#' @return NMDS Plot
#'
#' @importFrom vegan vegdist metaMDS
#' @importFrom ggplot2 ggplot stat_ellipse aes_string theme_classic
#' @examples
#' \dontrun{TaxaNorm_NMDS(TaxaNorm_Example_Output, group_column = "body_site")}
#'
#' @export

TaxaNorm_NMDS <- function(TaxaNormResults,group_column){

  mynorm <- normdata(TaxaNormResults)


  mynorm[sapply(mynorm, is.infinite)] <- NA

  cols <- sapply(mynorm, is.numeric)
  mynorm[cols] <- lapply(mynorm[cols], function(x)
    replace(x, is.na(x), min(x, na.rm = TRUE)))



  mydata <- input_data(TaxaNormResults)

  #take the lowest non inf number, replace -inf with lowest value
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
