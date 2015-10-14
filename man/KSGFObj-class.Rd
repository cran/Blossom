\name{KSGFObj-class}
\Rdversion{1.1}
\docType{class}
\alias{KSGFObj-class}
\alias{print,KSGFObj-method}
\alias{show,KSGFObj-method}
\alias{summary,KSGFObj-method}

\title{Class "KSGFObj"}
\description{
The \code{KSGFObj} class holds model specification and output from
 the coverage test.  The \code{summary.KSGFObj} class
represents the summary of these objects.
}

\section{Objects from the Class}{Objects can be created by calls of the
  form \code{new("KSGFObj", ...)}  or, more commonly, via the
  \code{\link{coverage}} function.
}

\section{Slots}{
  \describe{
    \item{\code{inputData}:}{A vector with the input variable.}
    \item{\code{ObsDelta}:}{The observed coverage statistic.}
    \item{\code{ExpectDelta}:}{The expected value of the coverage statistic.}
    \item{\code{VarDelta}:}{The variance of the coverage statistic estimated using resampling.}
    \item{\code{StandDelta}:}{The standardized t-statistic.}
    \item{\code{DeltaSkew}:}{The skewness of the t-statistic.}
    \item{\code{P_value}:}{The p-value of the observed statistic.}
    \item{\code{DoArc}:}{A logical indicating whether a circular distribution was used.}
    \item{\code{ArcInterv}:}{How many units describe the circular units of measure recorded.}
    \item{\code{NumCases}:}{The total number of observations used in the analysis.}
    \item{\code{Call}:}{The original function call.}
  }
}
\section{Methods}{
  \describe{
  \item{print}{\code{signature(x = "KSGFObj")}: Prints a terse summary of the Coverage test.}
    \item{pvalue}{\code{signature(x = "KSGFObj")}: Extracts the Pearson type III p-value.}
    \item{show}{\code{signature(object = "KSGFObj")}: Same as print.}
    \item{summary}{\code{signature(object = "KSGFObj")}: Prints a detailed summary of the Coverage test.}
	 }
}
\references{
Rao, J.S. 1976. Some tests based on arc-lengths for the circle. \emph{Sankhya}, Series B \bold{38}, 329--338.

Sherman, B. 1950. A random variable related to the spacing of sample values. \emph{Annals of Mathematical Statistics} \bold{21}, 339--361.
}

\seealso{
\code{\link{coverage}}
}
\examples{
Out <- coverage(variable = rao$angl,interv = 360)
summary(Out)
}
\keyword{classes}
