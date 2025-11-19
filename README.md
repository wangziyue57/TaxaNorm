# TaxaNorm <img src="man/figures/logo-hex.png" width="150" align="right" />
TaxNorm is the R package for a novel feature-wise normalization method for microbiome data based on zero-inflated negative binomial model. This method assumes that the effects of sequencing depth vary for each taxon on their mean and incorporates a rational link of zero probability and taxon dispersion as a function of sequencing depth to better capture the microbiome data nature.

## Installation


Current stable release:

```r
install.packages("TaxaNorm")
```

Build from GitHub:

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("wangziyue57/TaxaNorm", 
                        dependencies = TRUE, 
                        build_vignettes = TRUE)
```



## Citation
Please cite our original paper: Wang Z, Lloyd D, Zhao S, Motsinger-Reif A. Taxanorm: a novel taxa-specific normalization approach for microbiome data. BMC bioinformatics. 2024 Sep 16;25(1):304.
