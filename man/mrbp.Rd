\name{mrbp}
\alias{mrbp}

\title{
Multiresponse permutation procedures for randomized blocks
}
\description{
Multiresponse permutation procedures for randomized blocks (MRBP) are used for
univariate and multivariate analyses of grouped data in a complete randomized
block design.
}
\usage{
mrbp(variables, group, block, data, expon = 1, exact = FALSE, number.perms,
  commens = TRUE, align = TRUE, save.test)
}

\arguments{
  \item{variables}{the names of response variables to be used in the analysis.  If more than one is used these are specified using the form \code{c(var1,var2,...).}}
  \item{group}{the name of the grouping variable to be used in the analysis.}
  \item{block}{the name of the blocking variable to be used in the analysis.}
  \item{data}{the \code{data.frame} or \code{matrix} containing columns with names matching all values supplied in the \code{variables}, \code{group}, and \code{block} arguments.
  Alternatively, if none of \code{variables}, \code{group}, and \code{block} are supplied, it is assumed
  that the first column is the grouping column, the second is the block column, and all remaining columns are variables to be used in the analysis.}
  \item{expon}{allows selection of alternative exponents in distance calculations.}
  \item{exact}{a \code{logical} value indicating whether to perform an exact test.  This is only available for <10 group combinations.}
  \item{number.perms}{number of permutations used if a Monte Carlo resampling procedure is selected.}
  \item{commens}{a \code{logical} value indicating whether to perform multivariable commensuration.  Commensuration can only be done when there is more than one variable.}
  \item{align}{A \code{logical} indicating whether the data should be aligned so that the median of all blocks is equal.}
  \item{save.test}{A \code{logical} indicating whether to save Monte Carlo resampling values of the test statistic (Delta).}
}

\details{
The value chosen to align each block is selected to make the block
medians all equal to zero.  Blossom aligns each block by default.
If there is more than one response
variable then Blossom adjusts or commensurates variables by
their average Euclidean distance by default as in \code{mrbp}.
}
\value{
\code{mrbp} returns an object of class \code{MRBPObj}.

The functions \code{\link{summary}} as well as \code{\link{print}} can be used to obtain a summary of the test.

Generic accessor functions \code{\link{pvalue}} and \code{\link{ResampVals}} can be used to obtain the p-value and Monte Carlo resampled test statistic values respectively.
}
\references{
Mielke, P.W., Jr., and K.J. Berry. 2001. \emph{Permutation methods: A distance function approach}. Springer-Verlag.
}


\seealso{
\code{\link{pvalue}} and \code{\link{ResampVals}}
}
\examples{
BiomassExamp <- mrbp(data = mrbp1,commens = FALSE,align = FALSE,save.test = TRUE)
summary(BiomassExamp)
#this just demonstrates another way of specifying the analysis 
BiomassExamp <- mrbp(variables = c(spp1,spp2,spp3),group = trtmt,block = block,data = mrbp1)
}
\keyword{nonparametric}
\keyword{multivariate}
\keyword{htest}
