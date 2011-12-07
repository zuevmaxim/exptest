\name{gini.exp.test}
\alias{gini.exp.test}
\title{Test for exponentiality based on the Gini statistic}
\description{
  Performs test for  the composite hypothesis of exponentiality based on the Gini statistic, 
  see e.g. Gail and Gastwirth (1978).
}
\usage{
gini.exp.test(x)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
}
\details{The test is based on the Gini statistic
\deqn{
G_n = \frac{\sum_{i,j=1}^n |X_i-X_j|}{2n(n-1)\overline{X}}.
}
The p-value is computed from the limit null distribution. Under exponentiality, the normalized statistic \eqn{(12(n-1))^{1/2}(G_n-0.5)} is asymptotically standard normal (see, e.g., Gail and Gastwirth (1978).
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the Gini statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Test for exponentiality based on the Gini statistic".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Gail, M.H. and Gastwirth, J.L. (1978): A scale-free goodness-of-fit test for the exponential distribution based on the Gini statistic. --- J. R. Stat. Soc. Ser. B, vol. 40, No. 3, pp. 350--357.
}
\author{Ruslan Pusev and Maxim Yakovlev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
gini.exp.test(rexp(100))
gini.exp.test(runif(100, min = 50, max = 100))
}
\keyword{htest}