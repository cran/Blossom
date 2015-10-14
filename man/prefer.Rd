\name{prefer}
\alias{prefer}
\docType{data}
\title{Habitat preference data for pheasants 
}
\description{Habitat preference data for 13 pheasants using 5 cover types (Aebischer et al. 1993) for randomized block analyses.
}
\usage{data(prefer)}
\format{
  A data frame with 26 observations on the following 7 variables.
  \describe{
    \item{\code{scrub}}{Percentage of scrub cover type.}
    \item{\code{broad}}{Percentage of broadleaf woodland cover type.}
    \item{\code{conifer}}{Percentage of conifer woodland cover type.}
    \item{\code{grass}}{Percentage of grass cover type.}
    \item{\code{crop}}{Percentage of crop cover type.}
    \item{\code{use}}{Grouping value taking a 1 if used by a bird and a 0 if available habitat.}
    \item{\code{bird}}{The numbers 1 to 13 indicating individual birds.}
  }
}
\details{In the randomized block analysis of this data, bird is the blocking factor and use is the treatment groups. Note the available habitat (use = 0) percentages are assumed constant across all birds.
}
\source{
Aebischer, N.J., P. A. Robertson, and R.E. Kenward. 1993. Compositional analysis of habitat use from animal radio-tracking data. \emph{Ecology} \bold{74}, 1313--1323.
}

\examples{
str(prefer)
  boxplot(prefer[1:13,1:5],main = "Percent habitat in the home range of 13 ring-necked pheasants")
     mtext("red dots indicate percent habitat available for each land cover class")
     points(seq(1:5),prefer[14,1:5],cex = 2,pch = 21,bg = "red")
}
\keyword{datasets}
