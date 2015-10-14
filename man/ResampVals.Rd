\name{ResampVals-methods}
\docType{methods}
\alias{ResampVals-methods}
\alias{ResampVals}
\alias{ResampVals,CoverageObj-method}
\alias{ResampVals,LADObj-method}
\alias{ResampVals,MRBPObj-method}
\alias{ResampVals,MRPPObj-method}
\alias{ResampVals,MRSPObj-method}
\alias{ResampVals,PTMPObj-method}
\title{Extract resample values}
\description{
This function extracts and reports resample values for several
objects in the Blossom library when their original call specified a permutation method.
}
\usage{
ResampVals(x)
}
\arguments{
  \item{x}{An object of a suitable class - usually an object of type
    \code{"\linkS4class{CoverageObj}"}, \code{"\linkS4class{LADObj}"}, \code{"\linkS4class{MRBPObj}"}, \code{"\linkS4class{MRPPObj}"},
     \code{"\linkS4class{MRSPObj}"}, or \code{"\linkS4class{PTMPObj}"}.}
}
\value{
  A vector of resampled values of the test statistic.
}
\examples{
print(bgrouse)
set.seed(10)
Out <- coverage(variable = bgrouse$distance,group = bgrouse$sex,
  number.perms = 100,save.test = TRUE)
summary(Out)
ResampVals(Out)[1:10]
}
\keyword{methods}

