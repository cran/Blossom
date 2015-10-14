\name{example3}
\alias{example3}
\docType{data}
\title{Example univariate response data for MRPP
}
\description{Hypothetical univariate response data in 2 groups with no outliers (Mielke 1986).
}
\usage{data(example3)}
\format{
  A data frame with 30 observations on the following 2 variables.
  \describe{
    \item{\code{group}}{Grouping variable (1 or 2).}
    \item{\code{response}}{Univariate response variable.}
  }
}

\source{
Mielke, P.W. 1986. Non-metric statistical analyses: some metric alternatives. \emph{Journal of Statistical Planning and Inference} \bold{13}, 377--387.
}

\examples{

par(mfrow=c(2,1))

hist(example3$response[example3$group==1],breaks = 6,xlim = c(14.5,16),
 ylim = c(0,7),ylab = "Count",xlab = "",main = "Group 1",col = "blue")

hist(example3$response[example3$group==2],breaks = 6,xlim = c(14.5,16),
 ylim = c(0,7),ylab = "Count",xlab = "Response",main = "Group 2",col = "blue")
}
\keyword{datasets}
