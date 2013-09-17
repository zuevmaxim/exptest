\name{moran.exp.test}
\alias{moran.exp.test}
\title{Moran test for exponentiality}
\description{
  Performs Moran test for the composite hypothesis of exponentiality, 
  see e.g. Moran (1951) and Tchirina (2005).
}
\usage{
moran.exp.test(x, simulate.p.value=FALSE, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{simulate.p.value}{a logical value indicating whether to compute p-values by Monte Carlo simulation.}
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Moran test for exponentiality is based on the following statistic:
\deqn{
T_n^+ = \gamma + \frac{1}{n}\sum_{i=1}^n{\log\frac{X_i}{\overline{X}}}, 
}
where \eqn{\gamma} is Euler-Mascheroni constant.
The statistic is asymptotically normal: \deqn{\sqrt{n}\,T_n^+ \to N\left(0,\frac{\pi^2}{6} - 1\right).}
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Moran statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Moran test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Moran, P.A.P. (1951): The random division of an interval--Part II. --- Journal of the Royal Statistical Society. Series B (Methodological), vol. 13, pp. 147-150.

Tchirina, A.V. (2005): Bahadur efficiency and local optimality of a test for exponentiality based on the Moran statistics. --- Journal of Mathematical Sciences, vol. 127, No. 1, pp. 1812--1819.

}

\author{Alexey Novikov and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
moran.exp.test(rexp(100))
moran.exp.test(rchisq(100,3))
}
\keyword{htest}