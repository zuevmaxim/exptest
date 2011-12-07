\name{ww.exp.test}
\alias{ww.exp.test}
\title{Wong and Wong test for exponentiality}
\description{
  Performs Wong and Wong test for the composite hypothesis of exponentiality, 
  see e.g. Ascher (1990).
}
\usage{
ww.exp.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The test is based on the following statistic:
\deqn{
Q=X_{(n)}/X_{(1)},
}
where   \eqn{X_{(1)}} and \eqn{X_{(n)}} are the smallest and the largest order statistics respectively.
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the statistic of the test.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Wong and Wong test for exponentiality".}
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
ww.exp.test(rexp(100))
ww.exp.test(abs(rcauchy(100)))
}
\keyword{htest}