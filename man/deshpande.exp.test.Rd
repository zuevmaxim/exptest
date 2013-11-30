\name{deshpande.exp.test}
\alias{deshpande.exp.test}
\title{Deshpande test for exponentiality}
\description{
  Performs Deshpande test for the composite hypothesis of exponentiality, see Deshpande (1983).
}
\usage{
deshpande.exp.test(x, b=0.44, simulate.p.value=FALSE, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{b}{a parameter of the test (see below).}
  \item{simulate.p.value}{a logical value indicating whether to compute p-values by Monte Carlo simulation.}
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The test is based on the following statistic:
\deqn{
J = \frac{1}{n(n - 1)}\, \sum_{i\ne j}1\{x_i > bx_j\}.
}
Under exponentiality, one has
\deqn{
\sqrt{n}(J-\frac{1}{b+1})\stackrel{d}{\rightarrow}\mathcal N\left(0,4\zeta_1\right),
}
where 
\deqn{
\zeta_1 = \frac{1}{4}\left(1+\frac{b}{b+2}+\frac{1}{2b+1}+\frac{2(1-b)}{b+1}-\frac{2b}{b^2+b+1}-\frac{4}{(b+1)^2} \right) 
}
(see Deshpande (1983)).
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value for the test.}
  \item{method}{the character string "Deshpande test for exponentiality".}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\references{Deshpande J.V. (1983): A class of tests for exponentiality against increasing failure rate average alternatives. --- Biometrika, vol. 70, pp. 514--518.
}
\author{Alexey Novikov and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
deshpande.exp.test(rexp(100))
deshpande.exp.test(rweibull(100,1.5))
}
\keyword{htest}