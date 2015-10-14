\name{sp}
\alias{sp}

\title{
Multiresponse sequence procedure
}
\description{
 Multiresponse sequence procedure (MRSP) is a special case of MRPP where first-order
 sequential pattern of data is tested against the null hypothesis of no sequential
 pattern. Univariate analyses are analogous to the Durbin-Watson test for first-order
 serial pattern and bivariate analyses are analogous to Schoener's \eqn{t^2/r^2} statistic (Solow 1989).
 Permutation versions of these two tests can be done. Options allow you to select
 the sequencing variable and to turn off multivariate commensuration.
}
\usage{
sp(data, expon = 1, commens = TRUE, number.perms, exact = FALSE,
save.test,sequence,variables)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{an object of class matrix (or an object coercible by \code{\link{as.matrix}}) with columns representing response variables.}
  \item{expon}{allows selection of alternative exponents in distance calculations.}
  \item{commens}{a logical value indicating whether to perform multivariate commensuration.}
  \item{number.perms}{number of permutations used if a Monte Carlo resampling procedure is to be used.}
  \item{exact}{logical indicating whether to perform an exact test or use a Monte Carlo resampling procedure.}
  \item{save.test}{logical indicating whether to return Monte Carlo resampled test statistic values.}
  \item{sequence}{a numeric vector specifying how the data should be ordered for the analysis.}
  \item{variables}{a character string of the names to be used in the analysis.  These should match names found in the \code{data.frame}.}
}
\details{
In this analysis of ungrouped data, the agreement measure (1 - average Euclidean distance
between ordered observations/average Euclidean distance among all possible pairs of observations)
is a statistic describing first-order serial dependency. Significance of the null hypothesis of no
first-order serial dependency is provided by the Pearson Type III approximation on the first 3
exact moments of the permutation distribution by default, optionally by exact enumeration for small
samples by specifying the \code{exact} option, or by a Monte Carlo resampling procedure by specifying
the \code{number.perms} option.
}
\value{
sp returns an object of class MRSPObj.

The functions \code{\link{summary}} as well as \code{\link{print}} can be used to obtain a summary of the test.

Generic accessor functions \code{pvalue} and \code{ResampVals} can be used to obtain the p-value and Monte Carlo resampled test statistic values respectively.
}
\references{
Solow, A.R. 1989. A randomization test for independence of animal locations. \emph{Ecology} \bold{70}, 1546--1549.
}

\seealso{
\code{\link{MRSPObj}}
}
\examples{
sp(blue162[,4:5], expon = 1,number.perms = 1000,save.test = TRUE)
}

\keyword{nonparametric}
\keyword{multivariate}
\keyword{htest}
