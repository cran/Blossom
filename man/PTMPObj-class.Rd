\name{PTMPObj-class}
\Rdversion{1.1}
\docType{class}
\alias{PTMPObj-class}
\alias{print,PTMPObj-method}
\alias{show,PTMPObj-method}
\alias{summary,PTMPObj-method}

\title{Class \code{"PTMPObj"}}
\description{The \code{PTMPObj} class holds model specification and output from
the Permutation test for matched pairs proceedure.  The \code{summary.PTMPObj} class
represents the summary of these objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PTMPObj", ...)} or, more commonly, via the
  \code{\link{ptmp}} function.
}

\section{Slots}{
  \describe{
    \item{\code{NumPairs}:}{The number of pairs found in the the input \code{data.frame} or \code{matrix}.}
    \item{\code{expon}:}{The exponent used in distance calculations.}
    \item{\code{Data1}:}{A vector representing the the responses for the first group.}
    \item{\code{Data2}:}{A vector representing the responses that match Data1 but for the second group.}
    \item{\code{ExpectDelta}:}{The expected value of Delta (only calculated for the Pearson type III approximation).}
    \item{\code{DeltaVar}:}{The variance of Delta (only calculated for the Pearson type III approximation).}
    \item{\code{DeltaSkew}:}{The skew of Delta (only calculated for the Pearson type III approximation).}
    \item{\code{StdStat}:}{The standardized test statistic.}
    \item{\code{Rho}:}{The agreement measure among blocks.}
    \item{\code{ObsDelta}:}{Delta Observed.}
    \item{\code{P_value}:}{The probability of a smaller or equal delta.}
    \item{\code{Resample}:}{A logical indicating whether resampling is performed.}
    \item{\code{Exact}:}{A logical indicating whether an exact test was performed.}
    \item{\code{NumPerms}:}{The number of permutations used in the analysis if specified in the function call.}
    \item{\code{Call}:}{The original function call.}
    \item{\code{SaveTest}:}{A logical indicating whether to save resample values of Delta.}
    \item{\code{PermVals}:}{If \code{SaveTest=TRUE} this holds the resample values of Delta.}
    \item{\code{NamesUsed}:}{If applicable the names used for the grouping, blocking and response variable.}
    \item{\code{GroupNames}:}{The names of the groups used in the analysis.}
    \item{\code{BlockNames}:}{The names of the blocks used in the analysis.}
    \item{\code{AlignVals}:}{The aligned values.}
  }
}

    
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "PTMPObj")}: Prints a terse summary of the PTMP test.}
    \item{pvalue}{\code{signature(x = "PTMPObj")}: Extracts the p-value.}
    \item{ResampVals}{\code{signature(x = "PTMPObj")}: Extracts the Monte Carlo resampled test statistic values.}
    \item{show}{\code{signature(object = "PTMPObj")}: Same as print.}
    \item{summary}{\code{signature(object = "PTMPObj")}:  Prints a detailed summary of the PTMP test.}
	 }
}

\author{
Marian Talbert
}


\seealso{
\code{\link{ptmp}}
}
\examples{
showClass("PTMPObj")
}
\keyword{classes}
