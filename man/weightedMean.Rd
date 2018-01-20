\name{weightedMean}
\alias{weightedMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of Outlier-Robust Weighted Mean
}
\description{
This function calculates the outlier-robust weighted mean in presence of outliers. A kernel weight function has been used in this function. 

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weightedMean(x, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numerical vector of metabolomic measurements.
%%     ~~Describe \code{a} here~~
}
  \item{b}{
value of tuning parameter. Default value is 0.2.
%%     ~~Describe \code{b} here~~
}
}

\value{
Computed the weighted mean.
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
x<-c(rnorm(200,3,1)) 
x1<-c(rnorm(180,3,1),rnorm(20,60,3)) #contain 20 outliers#
weightedMean(x)
mean(x)
weightedMean(x1)
mean(x1)

}