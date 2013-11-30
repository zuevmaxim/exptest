\name{ahsanullah.exp.test}
\alias{ahsanullah.exp.test}
\title{Test for exponentiality based on Ahsanullah characterization}
\description{
  Performs test for  the composite hypothesis of exponentiality based on the Ahsanullah characterization,
  see Volkova and Nikitin (2013).
}
\usage{
ahsanullah.exp.test(x, simulate.p.value=FALSE, nrepl=2000)
}
\arguments{
  \item{x}{a numeric vector of data values.} %Missing values are allowed.
  \item{simulate.p.value}{a logical value indicating whether to compute p-values by Monte Carlo simulation.}
  \item{nrepl}{the number of replications in Monte Carlo simulation.}
}
\details{The test is based on the following statistic:
\deqn{
I_n=\int_0^{\infty}(H_n(t)-G_n(t))dF_n(t),
}
where \eqn{F_n} is the empirical distribution function,
\deqn{
H_n(t) = \frac{1}{n^2}\sum_{i,j=1}^n 1\{|X_i-X_j|<t\}, \quad t\geq 0,
}
\deqn{
G_n(t) = \frac{1}{n^2}\sum_{i,j=1}^n 1\{2\min (X_i,X_j)<t\}, \quad t\geq 0.
}
Under exponentiality, one has
\deqn{
\sqrt{n}I_n\stackrel{d}{\rightarrow}\mathcal N\left(0,\frac{647}{4725}\right)
}
(see Volkova and Nikitin (2013)).
}
\value{
  A list with class "htest" containing the following components: 
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value for the test.}
  \item{method}{the character string "Test for exponentiality based on Ahsanullah characterization".}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Volkova K. Yu., Nikitin Ya. Yu. (2013): Exponentiality tests based on Ahsanullah's characterization and their efficiency. --- Zap. Nauchn. Sem. POMI, vol. 412, pp. 69--87 (in Russian); to be transl. in J. Math. Sci. (N.Y.).
}
\author{Alexey Novikov and Ruslan Pusev}

%\seealso{\code{\link[ctest]{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
%\code{\link{cvm.test}}, \code{\link{lillie.test}}, 
%\code{\link{pearson.test}}, \code{\link{sf.test}} for performing further tests for normality.
%\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
ahsanullah.exp.test(rexp(25))
ahsanullah.exp.test(rgamma(25,2))
}
\keyword{htest}