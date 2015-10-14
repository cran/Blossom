\name{ex4rank}
\alias{ex4rank}
\docType{data}
\title{Example univariate data for MRPP
}
\description{Hypothetical univariate response data to be compared in 2 groups.
}
\usage{data(ex4rank)}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{group}}{Grouping variable (1 or 2).}
     \item{\code{rank}}{Rank transformation of response variable.}
    \item{\code{response}}{Hypothetical response variables.}
   
  }
}

\source{
Mielke, P.W. 1986. Nonmetric statistical analyses: some metric alternatives. \emph{Journal of Statistical Planning and Inference} \bold{13}, 377--387.
}

\examples{
par(mfrow = c(2,1))

hist(ex4rank$rank[ex4rank$group==1],breaks = 10,xlim = c(0,30),
  ylim = c(0,7),ylab = "Count",xlab = "",main = "Group 1",col = "blue")
  
hist(ex4rank$rank[ex4rank$group==2],breaks = 20,xlim = c(0,30),
  ylim = c(0,7),ylab = "Count",xlab = "Rank",main = "Group 2",col = "blue")

}
\keyword{datasets}
