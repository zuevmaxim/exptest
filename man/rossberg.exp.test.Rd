\name{rossberg.exp.test}
\alias{rossberg.exp.test}
\title{Test for exponentiality based on Rossberg characterization}
\description{
  Performs test for  the composite hypothesis of exponentiality based on the Rossberg characterization,
  see Volkova (2010).
}
\usage{
rossberg.exp.test(x)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
}
\details{The test is based on the following statistic:
\deqn{
S_n=\int_0^\infty(H_n(t)-G_n(t))dF_n(t),
}
where \eqn{ F_n} is the empirical distribution function,
\deqn{
H_n(t) = (C_n^3)^{-1}\sum_{1\leq i<j<k\leq n}1\{X_{2,\{i,j,k\}}-X_{1,\{i,j,k\}}<t\}, \quad t\geq 0,
}
\deqn{
G_n(t) =(C_n^2)^{-1}\sum_{1\leq i<j\leq n}1\{\min (X_i,X_j)<t\}, \quad t\geq 0.
}
Here \eqn{X_{s,\{i,j,k\}}}, \eqn{s=1,2}, denotes the \eqn{s}th order statistic of \eqn{X_i,X_j,X_k}.
The p-value is computed from the limit null distribution. Under exponentiality, one has
\deqn{
\sqrt{n}S_n\stackrel{d}{\rightarrow}\mathcal N\left(0,\frac{52}{1125}\right)
}
(see, Volkova (2010)).
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the test statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Test for exponentiality based on Rossberg characterization".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Volkova, K.Yu. (2010): On asymptotic efficiency of exponentiality tests based on Rossberg characterization. --- J. Math. Sci., vol. 167, No. 4, pp. 486--494.
}
\author{Ruslan Pusev and Maxim Yakovlev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
rossberg.exp.test(rexp(25))
rossberg.exp.test(runif(25, min = 50, max = 100))
}
\keyword{htest}