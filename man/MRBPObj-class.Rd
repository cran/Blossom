\name{MRBPObj-class}
\Rdversion{1.1}
\docType{class}
\alias{MRBPObj-class}
\alias{print,MRBPObj-method}
\alias{show,MRBPObj-method}
\alias{summary,MRBPObj-method}

\title{Class \code{"MRBPObj"}}
\description{The \code{MRBPObj} class holds model specification and output from
the Multiresponse Randomized Block procedure.  The \code{summary.MRBPObj} class
represents the summary of these objects.

}
\section{Objects from the Class}{Objects can be created by calls of the
  form \code{new("MRBPObj", ...)}  or, more commonly, via the
  \code{\link{mrbp}} function.
}
\section{Slots}{
  \describe{
    \item{\code{inputData}:}{The \code{data.frame} used in the analysis.}
    \item{\code{DistExp}:}{The distance exponent used in calculation of distances between pairs of observations.}
    \item{\code{NumVars}:}{The number of variables specified in the function call.}
    \item{\code{NumBlocks}:}{The number of blocks in the blocking variable.}
    \item{\code{NumGrps}:}{The number of groups in the grouping variable.}
    \item{\code{NumPerm}:}{The number of permutations used in the analysis if specified in the function call.}
    \item{\code{NumObs}:}{The total number of observations in the \code{data.frame}.}
    \item{\code{AlignVals}:}{The block alignment values (if \code{align=TRUE})}
    \item{\code{Align}:}{A logical indicating whether the data has been aligned so that the median of all blocks is equal.}
    \item{\code{Exact}:}{A logical indicating whether an exact test was performed.}
    \item{\code{Resample}:}{A logical indicating whether resampling is performed.}
    \item{\code{Commensurate}:}{A logical indicating whether the data were commensurated.}
    \item{\code{ObsDelta}:}{Delta Observed.}
    \item{\code{ExpectDelta}:}{The expected value of Delta (only calculated for the Pearson type III moments approximation).}
    \item{\code{DeltaVar}:}{The variance of Delta (only calculated for the Pearson type III moments approximation).}
    \item{\code{DeltaSkew}:}{The skew of Delta (only calculated for the Pearson type III moments approximation).}
    \item{\code{AgreeVal}:}{The agreement measure among blocks.}
    \item{\code{StdStat}:}{The standardized test statistic.}
    \item{\code{P_value}:}{The probability of a smaller or equal delta.}
    \item{\code{Call}:}{The original function call.}
    \item{\code{CommenAvgDist}:}{If commensuration was done these are the commensurate average distances.}
    \item{\code{SaveTest}:}{A logical indicating whether to save resample values of Delta.}
    \item{\code{PermVals}:}{If \code{save.test=TRUE} this holds the resample values of Delta.}
    \item{\code{group.names}:}{The names of the groups used in the analysis.}
  }
}
\section{Methods}{
  \describe{
   \item{print}{\code{signature(x = "MRBPObj")}: Prints a terse summary of the MRBP test.}
    \item{pvalue}{\code{signature(x = "MRBPObj")}: Extracts the p-value.}
    \item{ResampVals}{\code{signature(x = "MRBPObj")}: Extracts the Monte Carlo resampled test statistic values.}
    \item{show}{\code{signature(object = "MRBPObj")}: Same as print.}
    \item{summary}{\code{signature(object = "MRBPObj")}: Prints a detailed summary of the MRBP test.}
	 }
}

\seealso{
\code{\link{mrbp}}
}
\examples{
set.seed(5)
a <- mrbp(data = mrbp1,number.perms = 1000,commens = FALSE,align = FALSE,save.test = TRUE)
ResampVals(a)[1:10]
summary(a)
}
\keyword{classes}
