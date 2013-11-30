\name{hollander.exp.test}
\alias{hollander.exp.test}
\title{Hollander-Proshan test for exponentiality}
\description{
  Performs Hollander-Proshan test for the composite hypothesis of exponentiality, see Hollander and Proshan (1972).
}
\usage{
hollander.exp.test(x, simulate.p.value=FALSE, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{simulate.p.value}{a logical value indicating whether to compute p-values by Monte Carlo simulation.}
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The test is based on the following statistic:
\deqn{
J_n = \frac{1}{n(n - 1)(n - 2)}\, \sum_{i\ne j,k; j<k}1\{x_i > x_j+x_k\}.
}
Under exponentiality, one has
\deqn{
\sqrt{n}(J_n-\frac{1}{4})\stackrel{d}{\rightarrow}\mathcal N\left(0,frac{5}{432}\right).
}
(see Hollander and Proshan (1972)).
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value for the test.}
  \item{method}{the character string "Hollander-Proshan test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Hollander M., Proshan F. (1972): Testing whether new is better than used. --- Ann. Math. Stat., vol. 43, pp. 1136--1146.
}
\author{Alexey Novikov and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
hollander.exp.test(rexp(25))
hollander.exp.test(rgamma(25,2))
}
\keyword{htest}