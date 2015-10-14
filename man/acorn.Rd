\name{acorn}
\alias{acorn}
\docType{data}
\title{Acorn Production}

\description{
Acorn production data as related to oak (\emph{Quercus spp.}) forest characteristics.
}
\usage{data(acorn)}
\format{
  A data frame with 43 observations on the following 2 variables.
  \describe{
    \item{\code{wtperha}}{Annual acorn biomass (kg/ha).}
    \item{\code{oakccsi}}{Forest suitability index based on canopy cover and number of oak species.}
  }
}

\source{
Schroeder, R.L. and L.D. Vangilder. 1997. Tests of wildlife habitat models to evaluate oak mast production. \emph{Wildlife Society Bulletin} \bold{25}, 639--646.

}
\references{
Cade, B.S., J.W. Terrell, and R.L. Schroeder. 1999. Estimating effects of limiting factors with regression quantiles. \emph{Ecology} \bold{80}, 311--323.
}
\examples{

str(acorn)
plot(acorn$oakccsi,acorn$wtperha,xlab = "Acorn suitability index",ylab = "Kg Acorns per ha")

}
\keyword{datasets}
