\name{frozini.exp.test}
\alias{frozini.exp.test}
\title{Frozini test for exponentiality}
\description{
  Performs Frozini test for the composite hypothesis of exponentiality, 
  see e.g. Frozini (1987).
}
\usage{
frozini.exp.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Frozini test for exponentiality is based on the following statistic:
\deqn{
B_n = \frac{1}{\sqrt{n}}\sum_{i=1}^n{\left| 1 - \exp(-X_{(i)}/\overline{X}) - \frac{i-0.5}{n} \right|}.
}
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Frozini statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Frozini test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Frozini, B.V. (1987): On the distribution and power of a goodness-of-fit statistic with parametric and nonparametric applications, "Goodness-of-fit". (Ed. by Revesz P., Sarkadi K., Sen P.K.) --- Amsterdam-Oxford-New York: North-Holland. --- Pp. 133--154.
}

\author{Alexey Novikov and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
frozini.exp.test(rexp(100))
frozini.exp.test(rchisq(100,2))
}
\keyword{htest}