\name{shapiro.exp.test}
\alias{shapiro.exp.test}
\title{Shapiro-Wilk test for exponentiality}
\description{
  Performs Shapiro-Wilk test for the composite hypothesis of exponentiality, 
  see e.g. Shapiro and Wilk (1972).
}
\usage{
shapiro.exp.test(x, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The Shapiro-Wilk test for exponentiality is based on the following statistic:
\deqn{
W =
\frac{n(\overline{X} - X_{(1)})^2}{(n - 1)\sum_{i=1}^n{(X_i - \overline{X})^2}}.
}
The p-value is computed by Monte Carlo simulation.
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Shapiro-Wilk statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Shapiro-Wilk test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Shapiro, S.S. and Wilk, M.B. (1972): An analysis of variance test for the exponential distribution (complete samples). --- Technometrics, vol. 14, pp. 355-370.
}
\author{Alexey Novikov and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
shapiro.exp.test(rexp(100))
shapiro.exp.test(rchisq(100,1))
}
\keyword{htest}