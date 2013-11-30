\name{co.exp.test}
\alias{co.exp.test}
\title{Test for exponentiality of Cox and Oakes}
\description{
  Performs Cox and Oakes test for  the composite hypothesis of exponentiality, 
  see e.g. Henze and Meintanis (2005, Sec. 2.5).
}
\usage{
co.exp.test(x, simulate.p.value=FALSE, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{simulate.p.value}{a logical value indicating whether to compute p-values by Monte Carlo simulation.}
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Cox and Oakes test is a test for the composite hypothesis of exponentiality. 
The test statistic is 
\deqn{
CO_n = n+\sum_{j=1}^n(1-Y_j)\log Y_j,
}
where \eqn{Y_j=X_j/\overline{X}}. \eqn{(6/n)^{1/2}(CO_n/\pi)} is asymptotically standard normal (see, e.g., Henze and Meintanis (2005, Sec. 2.5)).
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Cox and Oakes statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Test for exponentiality based on the statistic of Cox and Oakes".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Henze, N. and Meintanis, S.G. (2005): Recent and classical tests for exponentiality: a partial review with comparisons. --- Metrika, vol. 61, pp. 29--45.
}
\author{Alexey Novikov, Ruslan Pusev and Maxim Yakovlev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
co.exp.test(rexp(100))
co.exp.test(runif(100, min = 0, max = 1))
}
\keyword{htest}