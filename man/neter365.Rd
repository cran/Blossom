\name{neter365}
\alias{neter365}
\docType{data}
\title{Soap scrap as a function of production line speed (Neter et al. 1989)
}
\description{Soap scrap as a function of production line speed for two different production lines.
}
\usage{data(neter365)}
\format{
  A data frame with 27 observations on the following 4 variables.
  \describe{
    \item{\code{soap}}{Soap scrap.}
    \item{\code{speed}}{Line speed.}
    \item{\code{line.n}}{An indicator of the production line.}
    \item{\code{lxs}}{Product of production line indicator and speed for their interaction.}
  }
}

\source{
Neter, J., W. Wasserman, and M.H. Kutner.  1989.  \emph{Applied Linear Regression Models}.  Richard S. Irwin, Inc., Homewood, Illinois. pp. 365--369.}

\references{
Cade, B.S., and J.D. Richards. 1996. Permutation tests for least absolute deviation regression. \emph{Biometrics} \bold{52}, 886--902.
}

\examples{
plot(neter365$speed,neter365$soap,col = c("red","blue")[unclass(neter365$line.n)+1],
  pch = c(17,19)[unclass(neter365$line.n)+1],ylab = "Soap scrap",
  xlab = "Production speed")
  
legend(x = 280,y = 220,pch = c(17,19),legend = c("line 0", "line 1"),col = c("red","blue"))

}
\keyword{datasets}
