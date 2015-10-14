\name{EMRPPObj-class}
\Rdversion{1.1}
\docType{class}
\alias{EMRPPObj-class}
\alias{EMRPPObj}
\alias{print,EMRPPObj-method}
\alias{show,EMRPPObj-method}
\alias{summary,EMRPPObj-method}

\title{Class \code{"EMRPPObj"}}
\description{The \code{EMRPPObj} class holds model specification and output from
the Multiresponse permutation procedure.  The \code{summary.EMRPPObj} class
represents the summary of these objects.
}
\section{Objects from the Class}{Objects can be created by calls of the
  form \code{new("EMRPPObj", ...)}  or, more commonly, via the
  \code{\link{mrpp}} function with \code{exact=TRUE} specification.
}

\section{Slots}{
  \describe{
    \item{\code{inputData}:}{A \code{data.frame} used for the analysis.  The first column is the group (if supplied) and the second is the variable for which the analysis was performed.}
    \item{\code{GpSizes}:}{A vector of length \code{NumGrps} specifying group sizes.}
    \item{\code{l_HasExcess}:}{A logical indicator of whether an excess group was used.}
    \item{\code{d_ExcessVal}:}{The excess value used in the analysis.}
    \item{\code{Hotelling}:}{A logical indicating Hotelling's variance/covariance standardization of the multiple dependent variables.}
    \item{\code{Commens}:}{A \code{logical} value indicating whether multivariable commensuration was performed.}
    \item{\code{MaxDist}:}{The maximum distance used in the analysis.  Inter-object distances greater than this value are replaced with this value.}
    \item{\code{Interval}:}{The number of units in the circular measure if a circular analysis was performed.}
    \item{\code{DistExp}:}{The exponent used in distance calculations.}
    \item{\code{CForm}:}{The value of the group weighting option that was used in the analysis.  See \code{\link{mrpp}} for more details.}
    \item{\code{NumGrps}:}{The number of groups in the input data.}
    \item{\code{NumObs}:}{The total number of observations in the original data set including those in the excess group.}
    \item{\code{NumVars}:}{The number of response variables used in the multiresponse sequence procedure.}
    \item{\code{da_GpVals}:}{A vector of group labels used internally in Fortran.}
    \item{\code{da_GroupV}:}{A vector with group labels used in Fortran for each observation.}
    \item{\code{ObsDelta}:}{The observed test statistic. This is the average of the observed intragroup distances weighted by relative group size.}
    \item{\code{YHot}:}{If Hotelling commensuration was specified this is the variance/covariance matrix.}
    \item{\code{P_value}:}{The exact probability of a smaller or equal delta under the null hypothesis.}
    \item{\code{Call}:}{The original function call.}
    \item{\code{group.names}:}{The names of the groups used in the analysis.}
    \item{\code{CommAvgDist}:}{If commensuration was applied this is a vector of commensurated distances.}
  }
}
\section{Methods}{
  \describe{
  \item{print}{\code{signature(x = "EMRPPObj")}: Prints a terse summary of the exact MRPP test.}
    \item{pvalue}{\code{signature(x = "EMRPPObj")}: Extracts the exact p-value.}
    \item{show}{\code{signature(object = "EMRPPObj")}: Same as print.}
    \item{summary}{\code{signature(object = "EMRPPObj")}: Prints a detailed summary of the exact MRPP test.}
	 }
}

\seealso{
\code{\link{mrpp}},  \code{\linkS4class{MRPPObj}}
}

\examples{
out <- mrpp(variables = c(distance,elev),group = sex,data = bgrouse,exact = TRUE)
summary(out)
}
\keyword{classes}
