\name{epstein.exp.test}
\alias{epstein.exp.test}
\title{Epstein test for exponentiality}
\description{
  Performs Epstein test for  the composite hypothesis of exponentiality, 
  see e.g. Ascher (1990).
}
\usage{
epstein.exp.test(x, simulate.p.value=FALSE, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{simulate.p.value}{a logical value indicating whether to compute p-values by Monte Carlo simulation.}
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The test is based on the following statistic:
\deqn{
EPS_n =\frac{2n\left(\log\left(n^{-1}\sum_{i=1}^nD_i\right)-n^{-1}\sum_{i=1}^n\log(D_i)\right)}{1+(n+1)/(6n)},
}
where \eqn{D_i=(n-i+1)(X_{(i)}-X_{(i-1)})}, \eqn{X_{(0)}=0} and \eqn{X_{(1)}\leq\ldots\leq X_{(n)}} are the order statistics.
Under exponentiality, \eqn{EPS} is approximately distributed as a chi-square with \eqn{n-1} degrees of freedom.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the test statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Epstein test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Ascher, S. (1990): A survey of tests for exponentiality. --- Communications in Statistics -- Theory and Methods, vol. 19, pp. 1811--1825.
}
\author{Alexey Novikov, Ruslan Pusev and Maxim Yakovlev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
epstein.exp.test(rexp(100))
epstein.exp.test(rweibull(100,2))
}
\keyword{htest}