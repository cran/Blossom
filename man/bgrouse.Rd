\name{bgrouse}
\alias{bgrouse}
\docType{data}
\title{Blue grouse migration data
}
\description{The distance and elevation change (in meters) for
male and female blue grouse (\emph{Dendragapus obscurus}) migrating
from where they were marked on their breeding range to their winter range.
}
\usage{data(bgrouse)}
\format{
  A data frame with 21 observations on the following 3 variables.
  \describe{
    \item{\code{sex}}{Bird gender, 4 = adult female, 3 = adult male.}
    \item{\code{distance}}{The distance traveled from their breeding to their winter range.}
    \item{\code{elev}}{The elevation change from breeding to winter range.}
  }
}

\source{
Cade, B.S., and R.W. Hoffman. 1993. Differential migration of blue grouse in Colorado. \emph{Auk} \bold{110}, 70--77.
}

\examples{
str(bgrouse)

plot(bgrouse$distance,bgrouse$elev,pch=c(17,19)[(bgrouse$sex==4)+1],
  col = c("red","blue")[(bgrouse$sex==4)+1],xlab = "Migration distance (m)",
  ylab = "Change in Elevation (m)",
  main = "Migration distance and elevation change for blue grouse")
  
legend(22000,150,c("Female","Male"),pch = c(17,19),col = c("red","blue"))
}
\keyword{datasets}
