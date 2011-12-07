\name{pietra.exp.test}
\alias{pietra.exp.test}
\title{Test for exponentiality based on the Pietra statistic}
\description{
  Performs test for  the composite hypothesis of exponentiality based on the Pietra statistic, 
  see e.g. Ascher (1990).
}
\usage{
pietra.exp.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The test is based on the Pietra statistic
\deqn{
P_n =\sum_{i=1}^n\frac{|X_i-\overline{X}|}{2n\overline{X}}.
}
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Pietra statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Test for exponentiality based on the Pietra statistic".}
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
pietra.exp.test(rexp(100))
pietra.exp.test(runif(100, min = 50, max = 100))
}
\keyword{htest}