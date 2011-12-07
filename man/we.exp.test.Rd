\name{we.exp.test}
\alias{we.exp.test}
\title{WE test for exponentiality}
\description{
  Performs the WE test for the composite hypothesis of exponentiality, 
  see e.g. Ascher (1990).
}
\usage{
we.exp.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The test is based on the following statistic
\deqn{
WE =\sum_{i=1}^n{(X_i-\overline{X})^2}/{\left(\sum_{i=1}^n{X_i}\right)^2}.
}
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the WE test statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "WE test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Ascher, S. (1990): A survey of tests for exponentiality. --- Communications in Statistics -- Theory and Methods, vol. 19, pp. 1811--1825.
}
\author{Ruslan Pusev and Maxim Yakovlev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
we.exp.test(rexp(100))
we.exp.test(runif(100, min = 50, max = 100))
}
\keyword{htest}