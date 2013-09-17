\name{atkinson.exp.test}
\alias{atkinson.exp.test}
\title{Atkinson test for exponentiality}
\description{
  Performs Atkinson test for the composite hypothesis of exponentiality, 
  see e.g. Mimoto and Zitikis (2008).
}
\usage{
atkinson.exp.test(x, p=0.99, simulate.p.value=FALSE, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{p}{a parameter of the test (see below).}
  \item{simulate.p.value}{a logical value indicating whether to compute p-values by Monte Carlo simulation.}
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Atkinson test for exponentiality is based on the following statistic:
\deqn{
T_n(p) = \sqrt{n}\left| \frac{\left(n^{-1}\sum_{i=1}^n{X_i^p}\right)^{1/p}}{\overline{X}} -(\Gamma(1+p))^{\frac{1}{p}}\right|.
}
The statistic is asymptotically normal: \eqn{T_n(p) \to \left| N(0,\sigma^2(p))\right|}, where
\deqn{
\sigma^2(p) = \left(\Gamma(1+p)\right)^{\frac{2}{p}}\left( -1 - \frac{1}{p^2} + \frac{\Gamma(1+2p)}{p^2\Gamma^2(1+p)}\right).
}
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Atkinson statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Atkinson test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Mimoto, N. and Zitikis, R. (2008): The Atkinson index, the Moran statistic, and testing exponentiality. --- J. Japan Statist. Soc., vol. 38, pp. 187--205.

}

\author{Alexey Novikov and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
atkinson.exp.test(rexp(100))
atkinson.exp.test(rchisq(100,3))
}
\keyword{htest}