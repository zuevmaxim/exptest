\name{ks.exp.test}
\alias{ks.exp.test}
\title{Kolmogorov-Smirnov test for exponentiality}
\description{
  Performs Kolmogorov-Smirnov test for  the composite hypothesis of exponentiality, 
  see e.g. Henze and Meintanis (2005, Sec. 2.1).
}
\usage{
ks.exp.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Kolmogorov-Smirnov test for exponentiality is based on the following statistic:
\deqn{
KS_n =\sup_{x\geq0}|F_n(x)-(1-\exp(-x))|,
}
where \eqn{F_n} is the empirical distribution function of the scaled data \eqn{Y_j=X_j/\overline{X}}. The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Kolmogorov-Smirnov statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Kolmogorov-Smirnov test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Henze, N. and Meintanis, S.G. (2005): Recent and classical tests for exponentiality: a partial review with comparisons. --- Metrika, vol. 61, pp. 29--45.
}
\author{Ruslan Pusev and Maxim Yakovlev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
ks.exp.test(rexp(100))
ks.exp.test(runif(100, min = 50, max = 100))
}
\keyword{htest}