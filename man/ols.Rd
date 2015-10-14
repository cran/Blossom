\name{ols}
\alias{ols}

\title{
Ordinary least squares regression
}
\description{
OLS regression estimates rates of change in conditional means.
The permutation testing approaches are identical to those used
for LAD regression, and are described in Kennedy and Cade (1996)
and Anderson and Legendre (1999). The test statistic is similar
in structure to that for LAD regression, except for OLS T_obs
equals (sum of squared residuals for
reduced parameter model - sum of squared residuals for full parameter model) / sum of squared residuals for full model.}

\usage{
ols(formula, data, contrasts,number.perms = 5000, quant,
   test = FALSE, all.quants = FALSE,weights)
}

\arguments{
 \item{formula}{an object of class \code{\link{formula}} or a statement that can be coerced to this class.}
  \item{data}{a data frame or object that can be coerced to one using \code{\link{as.data.frame}} containing the variables and response in the model.
    If missing the formula is evaluated in the environment from which the original function call was made.}
  \item{contrasts}{an optional list see \code{\link{contrasts}} for further details.}
  \item{number.perms}{number of permutations used if a Monte Carlo resampling procedure is to be used.}
  \item{quant}{a numeric value which specifies a quantile for which all subsequent testing is done.}
  \item{test}{a logical indicating whether to test if all slope parameters are equal to zero.}
  \item{all.quants}{a logical indicating whether all possible quantile regression estimates should be returned.}
   \item{weights}{a vector of weights to be applied to the response and model frame matrix.}
}
\details{Generally called indirectly via the \code{\link{lad}} function call with the option \code{OLS} set equal to \code{TRUE}.
}
\value{
\code{ols} returns an object of class \code{LADObj}.
}
\references{
Anderson, M. J., and P. Legendre. 1999. An empirical comparison of permutation methods for
tests of partial regression coefficients in a linear model. \emph{Journal Statistical Computation and Simulation} \bold{62}, 271--303.

Kennedy, P.E., and B.S. Cade. 1996. Randomization tests for multiple regression. \emph{Communications in Statistics - Simulation and Computation} \bold{25}, 923--936.
}

\seealso{
\code{\link{lad}} and \code{\link{hypothesis.test}}
}
\examples{
Out <- ols(lcc~-1+apico+picopha,data = fraserf,number.perms = 10000)
summary(Out)}


\keyword{htest}

