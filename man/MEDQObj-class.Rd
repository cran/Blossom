\name{MEDQObj-class}
\Rdversion{1.1}
\docType{class}
\alias{MEDQObj}
\alias{MEDQObj-class}
\alias{print,MEDQObj-method}
\alias{show,MEDQObj-method}
\alias{summary,MEDQObj-method}

\title{Class \code{"MEDQObj"}}
\description{The \code{MEDQObj} class holds model specification and output from
the Multivariate Medians and Distance Quantiles procedure.  The \code{summary.MEDQObj} class
represents the summary of these objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MEDQObj", ...)} or, more commonly, via the
  \code{\link{medq}} function.
}
\section{Slots}{
  \describe{
    \item{\code{NumGrps}:}{The number of groups in the input data.}
    \item{\code{NumVars}:}{The number of response variables used in the multivariate medians and distance quantiles.}
    \item{\code{NumObs}:}{The total number of observations in the original data set.}
    \item{\code{GrpSizes}:}{A vector of length \code{NumGrps} specifying group sizes.}
    \item{\code{inputData}:}{A \code{data.frame} used for the analysis.}
    \item{\code{NumQuantVals}:}{The number of quantiles specified in the analysis.}
    \item{\code{quant}:}{A vector of quantiles used in the analysis.}
    \item{\code{MaxGpSize}:}{The number of observations in the largest group.}
    \item{\code{VariableWInGpMedian}:}{ Within group median coordinates for specified variables.}
    \item{\code{GpAvgDistToGpMVMedian}:}{Average distance to multivariate median for each group.}
    \item{\code{GpMedQTolerance}:}{The solution tolerance.}
    \item{\code{WInGpQuantDist}:}{Within group distance quantile distance from the median.}
    \item{\code{ObsDistToGpMedian}:}{The observed distance to the multivariate median.}
    \item{\code{NumIterations}:}{The number of iterations for convergence to be reached.}
    \item{\code{Call}:}{The original function call.}
    \item{\code{group.names}:}{The names of the groups used in the analysis.}
  }
}
\section{Methods}{
  \describe{
    \item{Dist2mvm}{\code{signature(x = "MEDQObj")}: appends the distance to the multivariate median onto the the original input data and returns it.}
    \item{print}{\code{signature(x = "MEDQObj")}: same as summary }
    \item{show}{\code{signature(object = "MEDQObj")}: same as summary}
    \item{summary}{\code{signature(object = "MEDQObj")}: Prints a detailed summary of the MEDQ analysis.}
	 }
}

\author{
Marian Talbert
}


\seealso{
\code{\link{medq}}
}
\examples{
out <- medq(data = bgrouse)
summary(out)
#to get the data originally stored in the save option use
BgDist2mvm <- Dist2mvm(out)
}
\keyword{classes}
