\name{MRPPObj-class}
\Rdversion{1.1}
\docType{class}
\alias{MRPPObj-class}
\alias{print,MRPPObj-method}
\alias{show,MRPPObj-method}
\alias{summary,MRPPObj-method}

\title{Class \code{"MRPPObj"}}
\description{The \code{MRPPObj} class holds model specification and output from
the Multiresponse permutation procedure.  The \code{summary.MRPPObj} class
represents the summary of these objects.
}

\section{Objects from the Class}{
  Objects can be created by calls of the
  form \code{new("MRPPObj", ...)}  or, more commonly, via the
  \code{\link{mrpp}} function.}

\section{Slots}{
  \describe{
    \item{\code{NumObs}:}{The total number of observations in the original data set including those in the excess group.}
    \item{\code{NumVars}:}{The number of response variables used in the multiresponse sequence procedure.}
    \item{\code{DistExp}:}{The exponent used in distance calculations.}
    \item{\code{MaxDist}:}{The maximum distance used in the analysis interobject distances greater than this value are replaced with this value.}
    \item{\code{CForm}:}{The value of the group weighting option that was used in the analysis.  See \code{\link{mrpp}} for more details.}
    \item{\code{Hotelling}:}{A \code{logical} indicating Hotelling's variance/covariance standardization of the multiple dependent variables.}
    \item{\code{Commens}:}{A \code{logical} value indicating whether multivariable commensuration was performed.}
    \item{\code{NumGrps}:}{The number of groups in the input data.}
    \item{\code{Interval}:}{The number of units in the circular measure if a circular analysis was performed.}
    \item{\code{GpSizes}:}{A vector of length \code{NumGrps} specifying group sizes.}
    \item{\code{NumPerm}:}{The number of permutations used.}
    \item{\code{DoResamp}:}{A \code{logical} value indicating whether a resample test was performed.}
    \item{\code{inputData}:}{A \code{data.frame} used for the analysis.  The first column is the group (if supplied) and the second is the variable for which the analysis was performed.}
    \item{\code{AvgDist}:}{A vector of average intragroup distances.  If groups are clustered these distances will be small compared to cases where the group members are spread out.}
    \item{\code{StandTestStat}:}{The standardized test statistic from the analysis}
    \item{\code{ObsDelta}:}{The observed test statistic. This is the average of the observed intragroup distances weighted by relative group size.}
    \item{\code{ExpectDelta}:}{The expected value of the test statistic under the null hypothesis.
    If \code{number.perms} was specified this is calculated using Monte Carlo resampling by randomly
    reassigning group labels otherwise an approximation is calculated using a Pearson III distribution.}
    \item{\code{DeltaVar}:}{If the Pearson III approximation was used this is the variance in Delta.}
    \item{\code{DeltaSkew}:}{If the Pearson III approximation was used this is the skew in Delta.}
    \item{\code{P_value}:}{The probability of a smaller or equal delta under the null hypothesis.}
    \item{\code{YHot}:}{If Hotelling commensuration was specified this is the variance/covariance matrix.}
    \item{\code{d_ExcessVal}:}{The excess value used in the analysis.}
    \item{\code{l_HasExcess}:}{A \code{logical} indicator of whether an excess group was used.}
    \item{\code{da_GpVals}:}{A vector of group labels used internally in Fortran.}
    \item{\code{da_GroupV}:}{A vector with group labels used in Fortran for each observation.}
    \item{\code{Call}:}{The original function call.}
    \item{\code{group.names}:}{The names of the groups used in the analysis.}
    \item{\code{SaveTest}:}{A \code{logical} indicating whether to return Monte Carlo resampled test statistic values.}
    \item{\code{PermVals}:}{A vector containing the observed test statistic followed by the permuted values of Delta under the null hypothesis.}
    \item{\code{CommAvgDist}:}{If commensuration was applied this is a vector of commensurated distances.}
  }
}
\section{Methods}{
  \describe{
  \item{print}{\code{signature(x = "MRPPObj")}: Prints a terse summary of the MRPP test.}
    \item{pvalue}{\code{signature(x = "MRPPObj")}: Extracts the Pearson type III or Monte Carlo resample p-value depending on the function specificatoin.}
    \item{ResampVals}{\code{signature(x = "MRPPObj")}: Extracts the Monte Carlo resampled test statistic values.}
    \item{show}{\code{signature(object = "MRPPObj")}: Same as print.}
    \item{summary}{\code{signature(object = "MRPPObj")}: Prints a detailed summary of the MRPP test.}
	 }
}


\seealso{
\code{\link{mrpp}}, \code{\link{EMRPPObj}}
}

\examples{
set.seed(3)
out <- mrpp(c(distance,elev),sex,data = bgrouse)
summary(out)
}
\keyword{classes}
