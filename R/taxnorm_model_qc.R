#' Function to QC TaxNorm algorithm
#' @name TaxNorm_Model_QC
#' @param TaxNormResults  Input data; Results from TaxNorm normalization
#'
#' @return a list containing qc taxnorm object
#'
#' @import phyloseq
#' @importFrom ggplot2 ggplot geom_density scale_color_brewer labs theme_classic
#' @examples
#' \dontrun{TaxNorm_Model_QC(TaxNormResults = TaxNorm_Example_Output)}
#' @export


TaxNorm_Model_QC <- function(TaxNormResults){

  Value <- NULL
  Phylum <- NULL

  mymodelpars <- model_pars(TaxNormResults)

  mycoefs <- coefficients(mymodelpars)

  myinput_data <- input_data(TaxNormResults)

  mytab <- table(tax_table(myinput_data)[, "Phylum"], exclude = NULL)

  mytaxa <- as.data.frame(tax_table(myinput_data)[, "Phylum"])

  myphy <- names(mytab)[-length(mytab)]


  mycoefs <- cbind(mycoefs,mytaxa)

  coef_depth <- list()


  for(i in 1:(length(mytab)-1)){

    coef_depth[[i]] <- mycoefs[which(mycoefs$Phylum == myphy[i]),2]

  }

  names(coef_depth) <- myphy




  plotdata <- data.frame(matrix(ncol = 2,nrow = 0))
  names(plotdata) <- c("Phylum","Phy")
  for(i in seq_along(coef_depth)){

    curr_data <- coef_depth[i]

    curr_data <- as.data.frame(curr_data)

    curr_data$Phy <- myphy[i]

    names(curr_data) <- c("Value","Phylum")

    plotdata <- rbind(plotdata,curr_data)


  }

  mean_coef_plot <- ggplot(plotdata, aes(x=Value, color=Phylum)) +
    geom_density() +
    scale_color_brewer(palette = "Dark2") +
    labs(x = "Slope for Sequencing Depth") +
    theme_classic()




  coef_zero <- list()

  for(i in 1:(length(mytab)-1)){

    coef_zero[[i]] <- mycoefs[which(mycoefs$Phylum == myphy[i]),8]

  }

  names(coef_zero) <- myphy




  plotdata <- data.frame(matrix(ncol = 2,nrow = 0))
  names(plotdata) <- c("Phylum","Phy")
  for(i in seq_along(coef_zero)){

    curr_data <- coef_zero[i]

    curr_data <- as.data.frame(curr_data)

    curr_data$Phy <- myphy[i]

    names(curr_data) <- c("Value","Phylum")

    plotdata <- rbind(plotdata,curr_data)


  }

  zero_coef_plot <- ggplot(plotdata, aes(x=Value, color=Phylum)) +
    geom_density() +
    scale_color_brewer(palette = "Dark2") +
    labs(x = "Slope for Sequencing Depth") +
    theme_classic()


  coef_disb <- list()

  for(i in 1:(length(mytab)-1)){

    coef_disb[[i]] <- mycoefs[which(mycoefs$Phylum == myphy[i]),10]

  }

  names(coef_disb) <- myphy

  plotdata <- data.frame(matrix(ncol = 2,nrow = 0))
  names(plotdata) <- c("Phylum","Phy")
  for(i in seq_along(coef_disb)){

    curr_data <- coef_disb[i]

    curr_data <- as.data.frame(curr_data)

    curr_data$Phy <- myphy[i]

    names(curr_data) <- c("Value","Phylum")

    plotdata <- rbind(plotdata,curr_data)


  }

  disb_coef_plot <- ggplot(plotdata, aes(x=Value, color=Phylum)) +
    geom_density() +
    scale_color_brewer(palette = "Dark2") +
    labs(x = "Slope for Sequencing Depth") +
    theme_classic()


return(list(mean_coef_plot,zero_coef_plot,disb_coef_plot))

}
