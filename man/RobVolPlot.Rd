\name{RobVolPlot}
\alias{RobVolPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Outlier-Robust Volcano Plot for Identifying Influential metabolites
%%  ~~function to do ... ~~
}
\description{
This function draws a volcano plot using the robust fold change value and the p-value obtained from robust t-test.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RobVolPlot(folds, pvals, cexcutoff = 0.7, cexlab = 0.5, 
plimit = 0.05, fclimit = 2, xlab = "log2 Robust Fold Change",
tab ylab = "-log10 Robust t-Test P-value", main = "Robust Volcano Plot", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{folds}{
A vector of fold changes with metabolite names.
%%     ~~Describe \code{folds} here~~
}
  \item{pvals}{
A vector of corresponding p-values with metabolite names. 
%%     ~~Describe \code{pvals} here~~
}
  \item{cexcutoff}{
Font size of the cut-off labels.
%%     ~~Describe \code{cexcutoff} here~~
}
  \item{cexlab}{
Font size of the variable labels.
%%     ~~Describe \code{cexlab} here~~
}
  \item{plimit}{
A numeric indicating the p value cutoff. The default is set to 0.05.
%%     ~~Describe \code{plimit} here~~
}
  \item{fclimit}{
A numeric indicating the lower fold cutoff. The default is set to 2.
%%     ~~Describe \code{fclimit} here~~
}
  \item{xlab}{
x-axis label.
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
y-axis label.
%%     ~~Describe \code{ylab} here~~
}
  \item{main}{
Plot title.
%%     ~~Describe \code{main} here~~
}
  \item{\dots}{
Other graphical parameters.
%%     ~~Describe \code{\dots} here~~
}
}

\author{
Nishith Kumar <nk.bru09@gmail.com>
%%  ~~who you are~~
}

\examples{
data(DummyFullData)
##Calculation of fold change values
fcval<-foldChngCalc(DummyFullData,nSampG1=20,nSampG2=20)
##Calculation of p-values
pval<-NULL
for (i in 1:dim(DummyFullData)[1]){
pval[i]<-p.valcalc(DummyFullData[i,1:20],DummyFullData[i,21:40])
}
##Robust volcano plot
RobVolPlot(fcval,pval , cexcutoff = 0.7, cexlab = 0.8, plimit = .05,fclimit = 2) 
}