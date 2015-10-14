\name{ptmp}
\alias{ptmp}

\title{
Permutation test for matched pairs
}
\description{
The permutation test for matched pairs (PTMP) is a special case of MRBP,
univariate data in two groups and n blocks, used for paired comparisons.
Options allows PTMP to perform permutation versions of paired t-tests and
Wilcoxon's signed rank test (for ranked data).
}
\usage{
ptmp(data,variables, expon = 1, exact = FALSE, number.perms, save.test)
}
 
\arguments{
\item{variables}{a character string of the names to be used in the analysis.  These should match names found in the \code{data.frame}.}
  \item{data}{the \code{data.frame} or \code{matrix} either with the first second and third columns representing the grouping variable, pairing variable and response variable respectively
    or a \code{data.frame} or \code{matrix} with two columns of pairs of responses.}
  \item{expon}{allows selection of alternative exponents in distance calculations.}
  \item{exact}{a logical value indicating whether to perform an exact test.}
  \item{number.perms}{if specified a Monte Carlo resampling procedure with \code{number.perms} permutations is to be used rather than a Pearson III approximation.}
  \item{save.test}{a logical which when true stores the permutation values of the test statistic.  This is only a valid option when \code{number.perms} is set.}
}

\details{The PTMP is statistically equivalent to MRBP either command can be used when pairing is indicating by the blocking variable but only
\code{ptmp} can be used when the observations are paired (next to each other) in the data set.
Thus the pairing is indicated by position or by a blocking variable.
The different test statistic structures produce an observed delta in PTMP that is exactly twice the observed test statistic for the same problem in MRBP.
Also, data in PTMP are aligned to a median of 0 by the structure of the test statistic.
}
\value{
\code{ptmp} returns an object of class \code{PTMPObj}.

}
\references{
Mielke, P.W., Jr., and K.J. Berry. 2001. \emph{Permutation methods: A distance function approach}. Springer-Verlag.
}
\author{
Marian Talbert
}



\seealso{
\code{\link{mrbp}}
}
\examples{

print(out <- ptmp(paired1,exact = TRUE))
pvalue(out)

}

\keyword{nonparametric}
\keyword{htest}

