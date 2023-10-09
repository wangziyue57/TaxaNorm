#' Function to run TaxNorm algorithm
#' @name TaxNorm_Run_Diagnose
#' @param Normalized_Results (Required) Input results from from run_norm()
#' @param prev run prev test
#' @param equiv run equiv test
#' @param group group used for taxanorm normalization
#' @return a list containing the normalized count values
#' @import phyloseq microbiome matrixStats
#' @examples
#' \dontrun{Diagnose_Data <- TaxNorm_Run_Diagnose(Normalized_Results = Normalized_Data,
#'                                         prev = TRUE,
#'                                         equiv = TRUE,
#'                                         group = sample_data(TaxNorm_Example_Input)$body_site)}
#' @export
#'
#'


TaxNorm_Run_Diagnose <- function(Normalized_Results, prev = TRUE, equiv = TRUE,group) {

  # extract info from run_norm() output (full model)
  res <- Normalized_Results
  count <- rawdata(res)

  llk.full <- llk(res)



  depth <- colSums(count)


  # fit reduced model
  # test 1: existence of effect or not; by fitting intercept-only regression
  if(prev){

    fit_reduce_1 <- list()
    for (i in 1:nrow(count)) {
      zero <- sum(count[i,]==0)

      if(zero >= 10){
        if(is.null(group)){
          fit_reduce_1[[i]] <- suppressWarnings(suppressWarnings(zinb.reg(formula = as.numeric(count[i,]) ~ 1 | 1 | 1,
                                                                          control = zinb.control(trace=FALSE, EM=TRUE))))
        }else{
          fit_reduce_1[[i]] <- suppressWarnings(zinb.reg(formula = as.numeric(count[i,]) ~ 1 + group | 1 | 1,
                                                         control = zinb.control(trace=FALSE)))
        }
      }
      if(zero < 10){
        if(is.null(group)){
          fit_reduce_1[[i]] <- suppressWarnings(suppressWarnings(nb.reg(formula = as.numeric(count[i,]) ~ 1 | 1,
                                                                        control = zinb.control(trace=FALSE, EM=TRUE))))
        }else{
          fit_reduce_1[[i]] <- suppressWarnings(suppressWarnings(nb.reg(formula = as.numeric(count[i,]) ~ 1 + group | 1,
                                                                        control = zinb.control(trace=FALSE, EM=TRUE))))
        }
      }
    }
    if (any(sapply(fit_reduce_1, inherits, "try-error")==TRUE)) {
      fit_reduce_1 <- fit_reduce_1[-which(sapply(fit_reduce_1, inherits, "try-error")==TRUE)]
    }
    llk.null <- sapply(fit_reduce_1, function(x) x$loglik)

    # lrt test
    na <- unique(c(which(is.na(llk.full)), which(is.na(llk.null))))
    if(length(na) == 0){
      llk_H1 <- llk.full
      llk_H0 <- llk.null

      df_H1 <- final_df(res)
      df_H0 <- sapply(fit_reduce_1, function(x) x$df.residual)

    }else{
      llk_H1 <- llk.full[-na]
      llk_H0 <- llk.null[-na]

      df_H1 <- df_H1 <- final_df(res)[-na]
      df_H0 <- sapply(fit_reduce_1, function(x) x$df.residual)[-na]
    }

    df <- sum(df_H0) - sum(df_H1)
    chi_stat <- 2 * (sum(llk_H1) - sum(llk_H0))
    pvalue_1 <- pchisq(chi_stat, df = df, lower.tail = FALSE)
  } else {pvalue_1 <- NA}


  # test 2: equivalence of effect or not; by fitting offset() term
  if(equiv){

    fit_reduce_2 <- list()
    for (i in 1:nrow(count)) {
      zero <- sum(count[i,]==0)

      if(zero >= 10){
        if(is.null(group)){
          fit_reduce_2[[i]] <- suppressWarnings(suppressWarnings(zinb.reg(formula = as.numeric(count[i,]) ~ offset(log(depth)) | 1 | 1,
                                                                          control = zinb.control(trace=FALSE, EM=TRUE))))
        }else{
          fit_reduce_2[[i]] <- suppressWarnings(zinb.reg(formula = as.numeric(count[i,]) ~ offset(log(depth)) + group | 1 | 1,
                                                         control = zinb.control(trace=FALSE)))
        }
      }
      if(zero < 10){
        if(is.null(group)){
          fit_reduce_2[[i]] <- suppressWarnings(suppressWarnings(nb.reg(formula = as.numeric(count[i,]) ~ offset(log(depth)) | 1,
                                                                        control = zinb.control(trace=FALSE, EM=TRUE))))
        }else{
          fit_reduce_2[[i]] <- suppressWarnings(suppressWarnings(nb.reg(formula = as.numeric(count[i,]) ~ offset(log(depth)) + group | 1,
                                                                        control = zinb.control(trace=FALSE, EM=TRUE))))
        }
      }
    }
    if (any(sapply(fit_reduce_2, inherits, "try-error")==TRUE)) {
      fit_reduce_2 <- fit_reduce_2[-which(sapply(fit_reduce_2, inherits, "try-error")==TRUE)]
    }
    llk.null <- sapply(fit_reduce_2, function(x) x$loglik)

    # lrt test
    na <- unique(c(which(is.na(llk.full)), which(is.na(llk.null))))
    if(length(na) == 0){
      llk_H1 <- llk.full
      llk_H0 <- llk.null

      df_H1 <- final_df(res)
      df_H0 <- sapply(fit_reduce_2, function(x) x$df.residual)

    }else{
      llk_H1 <- llk.full[-na]
      llk_H0 <- llk.null[-na]

      df_H1 <- df_H1 <- final_df(res)[-na]
      df_H0 <- sapply(fit_reduce_2, function(x) x$df.residual)[-na]
    }

    df <- sum(df_H0) - sum(df_H1)
    chi_stat <- 2 * (sum(llk_H1) - sum(llk_H0))
    pvalue_2 <- pchisq(chi_stat, df = df, lower.tail = FALSE)
  }else {pvalue_2 <- NA}

  return(list(p_prev = pvalue_1,
              p_equiv = pvalue_2))
}



