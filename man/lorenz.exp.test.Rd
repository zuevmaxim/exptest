\name{lorenz.exp.test}
\alias{lorenz.exp.test}
\title{Lorenz test for exponentiality}
\description{
  Performs Lorenz test for the composite hypothesis of exponentiality, 
  see e.g. Gail and Gastwirth (1978).
}
\usage{
lorenz.exp.test(x, p=0.5, simulate.p.value=FALSE, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{p}{a parameter of the test (see below).}
  \item{simulate.p.value}{a logical value indicating whether to compute p-values by Monte Carlo simulation.}
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Lorenz test for exponentiality is based on the following statistic:
\deqn{
L = \sum_{i=1}^{np}{X_{(i)}}/\sum_{i=1}^n{X_{(i)}}
}

The statistic \eqn{\sqrt{n}(L-p-(1-p)\log(1-p))} is asymptotically standard normal.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Lorenz statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Lorenz test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Gail, M.H. and Gastwirth, J.L. (1978): A scale-free goodness-of-fit test for the exponential distribution based on the Lorenz curve. --- Journal of the American Statistical Association, vol. 73, pp. 787--793.

}

\author{Alexey Novikov and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
lorenz.exp.test(rexp(100))
lorenz.exp.test(rchisq(100,7))
}
\keyword{htest}