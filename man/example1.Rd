\name{example1}
\alias{example1}
\docType{data}
\title{Example bivariate data for MRPP}
\description{Hypothetical bivariate responses in 2 groups from Biondini et al. (1985).
}
\usage{data(example1)}
\format{
  A data frame with 7 observations on the following 3 variables.
  \describe{
    \item{\code{group}}{Grouping variable (1 or 2).}
    \item{\code{x_coord}}{Response variable x.}
    \item{\code{y_coord}}{Response variable y.}
  }
}

\source{
Biondini, M.E., C.D. Bonham, and E.F. Redente. 1985. Secondary successional patterns in a sagebrush (\emph{Artemisia tridentata}) community
as they relate to soil disturbance and soil biological activity. \emph{Plant Ecology} \bold{60}, 25--36.
}

\examples{

plot(example1$x_coord,example1$y_coord,
 col = c("red","blue")[(example1$group==1)+1],
 cex = 2,pch = c(19,8)[(example1$group==1)+1],xlab = "X Coordinate",
 ylab = "Y Coordinate",main = "Example MRBP data set",cex.lab = 1.3,xlim = c(1,5),
 ylim = c(0,6))

legend(x = 1,y = 6,legend = c("Group 1","Group 2"),col = c("red","blue"),
 pch = c(19,8),cex = 1.5)

}
\keyword{datasets}
