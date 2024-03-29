% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxanorm_run_diag.R
\name{TaxaNorm_Run_Diagnose}
\alias{TaxaNorm_Run_Diagnose}
\title{Function to run TaxNorm algorithm}
\usage{
TaxaNorm_Run_Diagnose(Normalized_Results, prev = TRUE, equiv = TRUE, group)
}
\arguments{
\item{Normalized_Results}{(Required) Input results from from run_norm()}

\item{prev}{run prev test}

\item{equiv}{run equiv test}

\item{group}{group used for taxanorm normalization}
}
\value{
a list containing the normalized count values
}
\description{
Function to run TaxNorm algorithm
}
\examples{
\donttest{data("TaxaNorm_Example_Input", package = "TaxaNorm")
data("TaxaNorm_Example_Output", package = "TaxaNorm")
Diagnose_Data <- TaxaNorm_Run_Diagnose(Normalized_Results = TaxaNorm_Example_Output,
                                        prev = TRUE,
                                        equiv = TRUE,
                                        group = sample_data(TaxaNorm_Example_Input)$body_site)}
}
