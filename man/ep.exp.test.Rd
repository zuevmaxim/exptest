\name{ep.exp.test}
\alias{ep.exp.test}
\title{Test for exponentiality of Epps and Pulley}
\description{
  Performs Epps and Pulley test for  the composite hypothesis of exponentiality, 
  see e.g. Henze and Meintanis (2005, Sec. 2.8.1).
}
\usage{
ep.exp.test(x)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
}
\details{The Epps and Pulley test is a test for the composite hypothesis of exponentiality. 
The test statistic is 
\deqn{
EP_n = (48n)^{1/2} \left(\frac1n\sum_{j=1}^n\exp(-Y_j)-\frac12\right),
}
where \eqn{Y_j=X_j/\overline{X}}. The p-value is computed from the limit null distribution: \eqn{EP_n} is asymptotically standard normal (see, e.g., Henze and Meintanis (2005, Sec. 2.8.1).
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Epps and Pulley statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "The test for exponentiality of Epps and Pulley".}
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
ep.exp.test(rexp(100))
ep.exp.test(runif(100, min = 50, max = 100))
}
\keyword{htest}