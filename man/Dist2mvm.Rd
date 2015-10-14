\name{Dist2mvm-methods}
\docType{methods}
\alias{Dist2mvm-methods}
\alias{Dist2mvm}
\alias{Dist2mvm,MEDQObj-method}
\title{Report distance to the multivariate median}
\description{
Appends the distance to the multivariate median onto the the original input data and returns it.
}
\usage{
Dist2mvm(x)
}
\arguments{
  \item{x}{an object of a suitable class - at present only
    \code{"\linkS4class{MEDQObj}"}.}
}
\value{
  A \code{data.frame} with the distance to the multivariate median appended to the end of the original input data.
}
\examples{
out <- medq(data = bgrouse)
summary(out)
BgDist2mvm <- Dist2mvm(out)
}
\keyword{methods}