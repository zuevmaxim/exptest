\name{harris.exp.test}
\alias{harris.exp.test}
\title{Harris modification of Gnedenko F-test}
\description{
  Performs Harris modification of Gnedenko F-test for  the composite hypothesis of exponentiality, 
  see e.g. Ascher (1990).
}
\usage{
harris.exp.test(x, R=length(x)/4, simulate.p.value=FALSE, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{R}{a parameter of the test (see below).}
  \item{simulate.p.value}{a logical value indicating whether to compute p-values by Monte Carlo simulation.}
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The test is based on the following statistic:
\deqn{
Q_n(R) =\frac{\left(\sum_{i=1}^RD_i+\sum_{i=n-R+1}^nD_i\right)/(2R)}{\sum_{i=R+1}^{n-R}D_i/(n-2R)},
}
where \eqn{D_i=(n-i+1)(X_{(i)}-X_{(i-1)})}, \eqn{X_{(0)}=0} and \eqn{X_{(1)}\leq\ldots\leq X_{(n)}} are the order statistics.
Under exponentiality, \eqn{Q_n(R)} has an F distribution with \eqn{4R}  and \eqn{2(n-2R)} degrees of freedom.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the test statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Harris modification of Gnedenko F-test".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Ascher, S. (1990): A survey of tests for exponentiality. --- Communications in Statistics -- Theory and Methods, vol. 19, pp. 1811--1825.
}
\author{Alexey Novikov, Ruslan Pusev and Maxim Yakovlev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
harris.exp.test(rexp(100))
harris.exp.test(rlnorm(100))
}
\keyword{htest}