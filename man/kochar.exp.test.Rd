\name{kochar.exp.test}
\alias{kochar.exp.test}
\title{Kochar test for exponentiality}
\description{
  Performs Kochar test for the composite hypothesis of exponentiality, 
  see e.g. Kochar (1985).
}
\usage{
kochar.exp.test(x, simulate.p.value=FALSE, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{simulate.p.value}{a logical value indicating whether to compute p-values by Monte Carlo simulation.}
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Kochar test for exponentiality is based on the following statistic:
\deqn{
T = \sqrt{\frac{108n}{17}}\frac{\sum_{i=1}^n{J(i/(n+1))X_{(i)}}}{\sum_{i=1}^n{X_i}},
}
where
\deqn{
J(u) = 2(1-u)[1-\log{(1-u)}] - 1.
}
The statistic \eqn{T} is asymptotically standard normal.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Kochar statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Kochar test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Kochar, S.C. (1985): Testing exponenttality against monotone failure rate average. --- Communications in Statistics -- Theory and Methods, vol. 14, pp. 381--392.

}

\author{Alexey Novikov and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
kochar.exp.test(rexp(100))
kochar.exp.test(rchisq(100,1))
}
\keyword{htest}