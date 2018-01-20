\name{Rvolcano-package}
\alias{Rvolcano-package}
\alias{Rvolcano}
\docType{package}
\title{
Robust Volcano Plot for Identifying Influential Metabolites in Presence of Outliers.
}
\description{
This package implements a kernel weight based robust volcano plot for identifying influential Metabolites from noisy metabolomics dataset. This method is outlier-robust because the kernel weight function produce very smaller weight for outlying cell of the data matrix.
}
\details{
  \tabular{ll}{
        Package: \tab Rvolcano\cr
        Type: \tab Package\cr
        Version: \tab 1.0\cr
        Date: \tab 2018-01-14\cr
        License: \tab GPL \cr
        Depends: \tab R (>=2.10)\cr
    }
	
	Package Rvolcano has the following functions:
    \tabular{ll}{
         weightedMean(): \tab This function calculates the outlier-robust weighted mean using the kernel-weight\cr                                  \tab function.
\cr
         weightedVar(): \tab This function calculates the outlier-robust weighted variance using the kernel-weight\cr                              \tab function.
\cr
         foldChngCalc(): \tab This function calculates outlier-robust fold change value using the kernel-weight \cr
                         \tab function.
\cr
         p.valcalc(): \tab This function calculates the p-value using the robust version of t-statistic.\cr
         RobVolPlot(): \tab This function draws a outlier-robust volcano plot using the robust fold change value \cr
                       \tab and robust t-test p-value.\cr
	   
    }
}
\author{
Nishith Kumar\cr
Maintainer: Nishith Kumar <nk.bru09@gmail.com>}

\keyword{ package }