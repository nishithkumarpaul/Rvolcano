\name{p.valcalc}
\alias{p.valcalc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
p-Value Calculation Using Robust t-Test
%%  ~~function to do ... ~~
}
\description{
This function calculates the p-value using the robust version of t-test. A kernel weight function has been used behind the test statistic to robustify the t-test. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
p.valcalc(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numerical vector of metabolomic measurements.
%%     ~~Describe \code{x} here~~
}
  \item{y}{
a numerical vector of metabolomic measurements.
%%     ~~Describe \code{y} here~~
}
}

\value{
p-value of robust t-test.
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

\examples{
x<-rnorm(20,3,2)
y<-rnorm(20,6,2)
p.valcalc(x,y)
}
