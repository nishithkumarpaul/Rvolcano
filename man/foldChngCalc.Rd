\name{foldChngCalc}
\alias{foldChngCalc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Robust Fold Change Calculation for Differential Calculation
%%  ~~function to do ... ~~
}
\description{
This function calculates the outlier-robust fold change value in presence of outliers. A kernel weight function has been used in this function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
foldChngCalc(data, nSampG1, nSampG2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a numerical vector or matrix of metabolomic measurements.
%%     ~~Describe \code{data} here~~
}
  \item{nSampG1}{
number of subjects in group-1.
%%     ~~Describe \code{nSampG1} here~~
}
  \item{nSampG2}{
number of subjects in group-2.
%%     ~~Describe \code{nSampG2} here~~
}
}

\value{
Calculate the fold change values.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Nishith Kumar <nk.bru09@gmail.com>
}

\examples{
data(DummyFullData)
fcval<-foldChngCalc(DummyFullData,nSampG1=20,nSampG2=20)
fcval
}