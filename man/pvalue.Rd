\name{pvalue-methods}
\docType{methods}
\alias{pvalue-methods}
\alias{pvalue}
\alias{pvalue,CoverageObj-method}
\alias{pvalue,EMRPPObj-method}
\alias{pvalue,KSGFObj-method}
\alias{pvalue,LADObj-method}
\alias{pvalue,MRBPObj-method}
\alias{pvalue,MRPPObj-method}
\alias{pvalue,MRSPObj-method}
\alias{pvalue,PTMPObj-method}
\title{Extract P-values}
\description{
This function extracts and reports P-values for several objects in the Blossom library.
}
\usage{
pvalue(x)
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
Out <- coverage(variable = bgrouse$distance,group = bgrouse$sex,
        number.perms = 10000,save.test = TRUE)
summary(Out)
pvalue(Out)

}
\keyword{methods}