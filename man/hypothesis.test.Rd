\name{hypothesis.test}
\alias{hypothesis.test}

\title{
Hypothesis testing for LAD objects
}
\description{
Used for testing all slope parameters (full model) equal to zero or to test subsets
of parameters (partial models) equal to zero by Monte Carlo resampling of the
permutation distribution (Cade and Richards 1996) for LAD fitted objects including those fit
using Least Absolute Deviation (LAD) regression,
regression quantiles, and Ordinary Least Squares Regression (OLS).
}
\usage{
hypothesis.test(object1, object2, number.perms = 5000, save.test = TRUE,
   double.permutation = FALSE, rank.score = FALSE)
}

\arguments{
  \item{object1}{an object containing the results of a LAD model fit.}
  \item{object2}{an object containing the results of a LAD model fit.}
  \item{number.perms}{number of permutations used if a Monte Carlo resampling procedure is to be used.}
  \item{save.test}{is a logical indicating \code{TRUE} allows the Monte Carlo resampled test
  statistics to be saved into a single column variable as part of
  the LAD object, where the first value is always the observed test
  statistic value.  These can be retrieved using the command
  \code{ResampVals(LadObj)}.}
  \item{double.permutation}{is a logical indicating \code{TRUE} provides double permutation for null models that
  are constrained through the origins for either the drop in dispersion permutation
  test or the \code{rank.score}.}
  \item{rank.score}{a logical value indicating if a quantile rank score test should be used.  See details below.}
}
\details{
Type I errors of the regression quantile rank score test are less
sensitive to heterogeneous error distributions because the statistic
is based on the sign of the residuals from the reduced parameter
null model and not their size. However, as Cade (2003) and Cade et al. (2005)
make abundantly clear, valid Type I error rates often will require
appropriate weighted estimates and test statistics.

}
\value{
\code{hypothesis.test} returns an object of class \code{LADObj}.

}
\references{
Cade, B.S., and J.D. Richards. 1996.  Permutation tests for least absolute deviation regression.  \emph{Biometrics} \bold{52}, 886--902.

Cade, B.S. 2003. Quantile regression models of animal habitat relationships. Ph.D dissertation Colorado State University, Fort Collins. 186pp.

Cade, B.S. 2005. Linear models: Permutation methods. Pages 1049--1054 in B. Everitt and D. Howell, eds. \emph{Encyclopedia of Statistics in the Behavioral Science}. Vol. 2. John Wiley and Sons.

Cade, B.S., J.D. Richards, and P.W. Mielke, Jr.  2006.  Rank score and permutation testing alternatives for regression quantile estimates.  \emph{Journal of Statistical Computation and Simulation} \bold{76}, 331--355. 

Cade, B.S., and J.D. Richards.  2006.  A permutation test for quantile regression.  \emph{Journal of Agricultural, Biological, and Environmental Statistics} \bold{11}, 106--126. 
}

\seealso{
\code{\link{lad}}, \code{\link{LADObj}} \code{\link{summary}}, and \code{\link{pvalue}}.

}
\examples{
Out <- lad(wtlnlctm~-1+wt+wtwidrat,data = lahontan,quant = .9)
Out1 <- lad(wtlnlctm~-1+wt,data = lahontan,quant = .9)
Out2 <- lad(wtlnlctm~-1+wtwidrat,data = lahontan,quant = .9)

Test.Out <- hypothesis.test(Out,Out1,double.permutation = TRUE)
summary(Test.Out)

Test.Out <- hypothesis.test(Out,Out2,double.permutation = TRUE)
summary(Test.Out)
}

\keyword{nonparametric}
\keyword{htest}