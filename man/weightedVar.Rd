\name{weightedVar}
\alias{weightedVar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of Outlier-Robust Weighted Variance
%%  ~~function to do ... ~~
}
\description{
This function calculates the outlier-robust weighted variance in presence of outliers. A kernel weight function has been used behind this function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weightedVar(x, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numerical vector of metabolomic measurements.
%%     ~~Describe \code{a} here~~
}
  \item{b}{
value of tuning parameter. Default value is 0.25.
%%     ~~Describe \code{b} here~~
}
}

\value{
Computed the weighted variance.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Nishith Kumar <nk.bru09@gmail.com>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{

set.seed(2345)
x<-c(rnorm(100,0,1)) 
x1<-c(rnorm(90,0,1),rnorm(10,50,3)) # contain 10 outliers#
weightedVar(x)
var(x)
weightedVar(x1)
var(x1)

}