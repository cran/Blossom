\name{CoverageObj-class}
\Rdversion{1.1}
\docType{class}
\alias{CoverageObj-class}
\alias{CoverageObj}
\alias{print,CoverageObj-method}
\alias{show,CoverageObj-method}
\alias{summary,CoverageObj-method}

\title{Class \code{"CoverageObj"}}
\description{The \code{CoverageObj} class is used for the storage of coverage tests results.
}
\section{Objects from the Class}{Objects can be created by calls of the
  form \code{new("CoverageObj", ...)}  or, more commonly, via the
  \code{\link{coverage}} function.
}

\section{Slots}{
  \describe{
    \item{\code{NumPerms}:}{The number of permutations used if a permutation test was performed.}
    \item{\code{DistExp}:}{The exponent used in calculation of distances.}
    \item{\code{NumGrps}:}{The number of groups in the input data.}
    \item{\code{GpSizes}:}{A vector of length \code{NumGrps} specifying group sizes.}
    \item{\code{inputData}:}{A \code{data.frame} used for the analysis.  The first column is the group (if supplied) and the second is the variable for which the analysis was performed.}
    \item{\code{ObsDelta}:}{The observed coverage statistic.}
    \item{\code{VarDelta}:}{The variance of the coverage statistic estimated using resampling.}
    \item{\code{ExpectDelta}:}{The expected value of the coverage statistic.}
    \item{\code{DeltaSkew}:}{The standard deviation of the variance of the coverage statistic.}
    \item{\code{Z_value}:}{The observed standardized coverage statistic.}
    \item{\code{Skt}:}{The skewness of observed coverage statistic.}
    \item{\code{P_value}:}{The Pearson Type III probability of a larger
                or equal coverage statistic.}
    \item{\code{PZ}:}{The resampled probability of a larger
                or equal coverage statistic.}
    \item{\code{NumObs}:}{The total number of observations used in the analysis.}
    \item{\code{PermVals}:}{If \code{save.test=TRUE} was specified this vector will hold resampled test statistic values.}
    \item{\code{exact}:}{A logical value indicating whether an exact test was performed.}
    \item{\code{group.names}:}{The names of the groups used in the analysis.}
    \item{\code{Call}:}{The original function call.}
  }
}
\section{Methods}{
  \describe{
  \item{print}{\code{signature(x = "CoverageObj")}: Prints a terse summary of the Coverage test.}
    \item{pvalue}{\code{signature(x = "CoverageObj")}: Extracts the Pearson type III p-value.  See \code{\link{pvalue}}.}
    \item{ResampVals}{\code{signature(x = "CoverageObj")}: Extracts the Monte Carlo resampled test statistic values.  See \code{\link{ResampVals}}.}
    \item{show}{\code{signature(object = "CoverageObj")}: Same as print.  See \code{\link{print}}.}
    \item{summary}{\code{signature(object = "CoverageObj")}: Prints a detailed summary of the Coverage test.}
	 }
}

\references{
Mielke, P.W., and Y.C. Yao. 1988. A class of multiple sample tests based on empirical coverages.
\emph{Annals of the Institute of Statistical Mathematics} \bold{40}, 165--178.

Mielke, P.W. and Y.C. Yao. 1990. On g-sample empirical coverage tests:
Exact and simulated null distributions of test statistics with small
and moderate sample sizes. \emph{Journal Statistical Computation and
Simulation} \bold{35}, 31--39.

}
\seealso{
\code{\link{coverage}}
}
\examples{

Out <- coverage(variable = bgrouse$distance,group = bgrouse$sex)
summary(Out)


}
\keyword{classes}
