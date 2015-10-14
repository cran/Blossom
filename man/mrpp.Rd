\name{mrpp}
\alias{mrpp}

\title{
Multiresponse permutation procedures
}
\description{Multiresponse permutation procedures (MRPP) are used for univariate and
multivariate analyses of grouped data in a completely randomized one-way design.
MRPP are used for comparing equality of treatment groups analogous to one-way analysis
of variance (or t-test) for univariate data, or multivariate analysis of variance
(Hotelling's T^2) for multivariate data.
}
\usage{
mrpp(variables, group, data, expon = 1, c.form = 1, hotelling = FALSE,
  commens = TRUE, interv = 0, number.perms, exact = FALSE,
  has.excess = FALSE, excess.value, max.dist, save.test)
}

\arguments{
  \item{variables}{the names of response variables to be used in the analysis.  If more than one is used these are specified using the form \code{c(var1,var2,...).}}
  \item{group}{the name of the grouping variable to be used in the analysis.}
  \item{data}{the \code{data.frame} or \code{matrix} containing columns with names matching all values supplied in the \code{variables} and \code{group} arguments.
  Alternatively, if neither \code{variables} and \code{group} are supplied, it is assumed
  that the first column is the grouping column, and all remaining columns are variables to be used in the analysis.}
  \item{expon}{allows selection of alternative exponents in distance calculations. Default uses 1 corresponding to Euclidean distance.  Use of 2 is squared Euclidean distance, corresponding to many conventional parametric tests on means.}
  \item{c.form}{has four options that control how the groups are weighted:
       \itemize{
        \item c.form = 1:  C(I) = n(I)/sum(n(I))
        \item c.form = 2:  C(I) = (n(I)-1)/sum(n(I)-1)
        \item c.form = 3:  C(I) = 1/sum(1)
        \item c.form = 4:  C(I) = (n(I)*(n(I)-1))/sum(n(I)*(n(I)-1))
      }}
  \item{hotelling}{a \code{logical} indicating Hotelling's variance/covariance standardization of the multiple dependent variables.}
  \item{commens}{a \code{logical} value indicating whether to perform average Euclidian distance commensuration of multiple response variables.
  Commensuration can only be done when there is more than one variable.}
  \item{interv}{allows an analysis to be conducted on univariate circular data such as time or compass orientation.
  This analysis recognizes that there are no endpoints to the measurement scale. \code{interv} should be set to the number of units in the circular measure.}
  \item{number.perms}{if specified a Monte Carlo resampling procedure with \code{number.perms} permutations is to be used rather than a Pearson III approximation.}
  \item{exact}{a \code{logical} value indicating whether to perform an exact test.  This is computationally intensive for >30 observations.}
  \item{has.excess}{a \code{logical} indicating whether there is an excess group.}
  \item{excess.value}{the value of the excess group, if not specified it is assumed that the largest grouping value indicates the excess group.}
  \item{max.dist}{specifying a numeric value causes the MRPP analysis to replace interobject distances \code{delta_(i,j)} greater than the truncation value with the truncation value.}
  \item{save.test}{a \code{logical} indicating to store the permutation values of the test statistic.  This is only a valid option when \code{number.perms} is set.}
}
\details{The default Euclidean distance function in MRPP provides
an omnibus test of distributional equivalence among groups or a test
for common medians if the assumption of equal dispersions is applicable.
Options allow MRPP to perform permutation (randomization) versions of t-tests,
one-way analysis of variance, Kruskal-Wallis tests (for ranked data),
Mann-Whitney Wilcoxon tests (for ranked data), and one-way multivariate
analysis of variance. Options in MRPP also allow you to truncate
distances to evaluate multiple clumping of data, establish an
excess group, and select arc distances to compare circular
distributions of grouped data. Multivariate data are commensurated
(standardized) to a common scale but an option allows you to turn
off commensuration. Commensuration can be done by using average
Euclidean distance (default) or the variance/covariance
matrix for the dependent variables. Multivariate medians and distance
quantiles (MEDQ) are provided as estimates to be used in describing
distributional changes detected by MRPP analyses.
}
\value{
\code{mrpp} returns an object of either class \code{MRPPObj} or \code{EMRPPObj}.

The functions \code{\link{summary}} as well as \code{\link{print}} can be used to obtain a summary of the test.

Generic accessor functions \code{\link{pvalue}} and \code{\link{ResampVals}} (for \code{MRPPObj}) can be used to obtain the p-value and Monte Carlo resampled test statistic values respectively.

}
\references{
Mielke, P.W., Jr., and K.J. Berry. 2001. \emph{Permutation methods: A distance function approach}. Springer-Verlag.
}

\seealso{
\code{\link{pvalue}}, and \code{\link{ResampVals}}
}
\examples{
out <- mrpp(variables = c(distance,elev),group = sex,data = bgrouse,
 exact = TRUE)
summary(out)
}

\keyword{nonparametric}
\keyword{multivariate}
\keyword{htest}
