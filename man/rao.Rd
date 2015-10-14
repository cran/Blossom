\name{rao}
\alias{rao}
\docType{data}
\title{Circular orientation of homing pigeons (Rao 1976)}

\description{The angular degree orientation of 10 homing pigeons used by Rao (1976) for his spacing test of uniformity on a circular distribution.
}
\usage{data(rao)}
\format{
  A data frame with 10 observations on the following variable.
  \describe{
    \item{\code{angl}}{Compass orientation in degrees.}
  }
}

\source{
Rao, J.S. 1976. Some tests based on arc-lengths for the circle. \emph{Sankhya, Series B} \bold{38},329--338.
}

\examples{

str(rao)
par(pty = "s")
plot(c(-1.3,1.3),c(-1.4,1.4),type = "n",xaxt = "n",yaxt = "n",xlab = "",ylab = "",
  main = "Pigeon Orientation")
  symbols(x = 0, y = 0, circles = 1.1,add = TRUE,fg = seq(1:10),inch = FALSE)
  points(unlist(sin(rao*pi/180)),unlist(cos(rao*pi/180)),pch = 19,cex = 1.5)
  mtext(text = c("North","East","South","West"), side = c(3,4,1,2))

  text(x = 0,y = 1.4,"0")
  text(x = 1.2,y = 0,"90")
  text(x = 0,y = -1.4,"180")
  text(x = -1.2,y = 0,"270")

}
\keyword{datasets}
