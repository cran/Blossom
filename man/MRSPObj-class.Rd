\name{MRSPObj-class}
\Rdversion{1.1}
\docType{class}
\alias{MRSPObj-class}
\alias{MRSPObj}
\alias{print,MRSPObj-method}
\alias{show,MRSPObj-method}
\alias{summary,MRSPObj-method}

\title{Class \code{"MRSPObj"}}

\description{The \code{MRSPObj} class holds model specification and output from
the Multiresponse sequence procedure.  The \code{summary.MRSPObj} class
represents the summary of these objects.
}
\section{Objects from the Class}{Objects can be created by calls of the
  form \code{new("MRSPObj", ...)}  or, more commonly, via the
  \code{\link{sp}} function.}
\section{Slots}{
  \describe{
    \item{\code{NumObs}:}{The total number of observations in the original data set.}
    \item{\code{NumVars}:}{The number of response variables used in the multiresponse sequence procedure.}
    \item{\code{DistExp}:}{The distance exponent used in calculation of distances between pairs of observations.}
    \item{\code{NumPerm}:}{The number of permutations use if a permutation test was performed.}
    \item{\code{DoResamp}:}{A \code{logical} value indicating whether a resample test was performed.}
    \item{\code{Exact}:}{A \code{logical} value indicating whether an exact test was performed.}
    \item{\code{Commens}:}{A \code{logical} value indicating whether the data were commensurated.}
    \item{\code{inputData}:}{The original \code{data.frame} or \code{matrix} containing the response variables for which the test was run.}
    \item{\code{TestStat}:}{The standardized test statistic.}
    \item{\code{ObsDelta}:}{The observed delta value based on distances between sequentially ordered locations.}
    \item{\code{ExpectDelta}:}{The average distance among locations ignoring serial dependence.}
    \item{\code{DeltaVar}:}{Delta variance.}
    \item{\code{DeltaSkew}:}{Delta Skewness.}
    \item{\code{RhoAgreement}:}{The agreement measure or percent reduction in
        average distance between sequentially ordered values (observed delta)
        over that expected without any order (expected delta).}
    \item{\code{P_value}:}{The p-value for the test.}
    \item{\code{Call}:}{The original function call.}
    \item{\code{SaveTest}:}{A \code{logical} indicating whether to return Monte Carlo resampled test statistic values.}
    \item{\code{PermVals}:}{If \code{save.test} was specified this vector will hold resampled test statistic values.}
    \item{\code{CommAvgDist}:}{If Commensurate was specified this vector contains commensurated values.}
  }
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "MRSPObj")}: Prints a terse summary of the MRSP test.}
    \item{pvalue}{\code{signature(x = "MRSPObj")}: Extracts the p-value.}
    \item{ResampVals}{\code{signature(x = "MRSPObj")}: Extracts the Monte Carlo resampled test statistic values.}
    \item{show}{\code{signature(object = "MRSPObj")}: Same as print.}
    \item{summary}{\code{signature(object = "MRSPObj")}: Prints a detailed summary of the MRSP test.}
	 }
}


\seealso{
\code{\link{sp}}
}
\examples{
sp.test <- sp(as.matrix(blue162[,4:5]), expon = 1,number.perms = 500)
summary(sp.test)
}
\keyword{classes}
