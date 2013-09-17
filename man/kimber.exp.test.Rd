\name{kimber.exp.test}
\alias{kimber.exp.test}
\title{Kimber-Michael test for exponentiality}
\description{
  Performs Kimber-Michael test for the composite hypothesis of exponentiality, 
  see e.g. Michael (1983), Kimber (1985).
}
\usage{
kimber.exp.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Kimber-Michael test for exponentiality is based on the following statistic:
\deqn{
D = \max_i{\left| r_i - s_i\right|},
}
where
\deqn{
s_i = \frac{2}{\pi} \, \arcsin{\sqrt{1-\exp(-X_{(i)}/\overline{X})}}, \qquad r_i = \frac{2}{\pi} \, \arcsin{\sqrt{(i - 0.5)/n}}.
}
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Kimber-Michael statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Kimber-Michael test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Kimber, A.C. (1985): Tests for the exponential, Weibull and Gumbel distributions based on the stabilized probability plot. --- Biometrika, vol. 72, pp. 661--663.

Michael, J.R. (1983): The stabilized probability plot. --- Biometrika, vol. 70, pp. 11--17.
}

\author{Alexey Novikov and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
kimber.exp.test(rexp(100))
kimber.exp.test(rchisq(100,2))
}
\keyword{htest}