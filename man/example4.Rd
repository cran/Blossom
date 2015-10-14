\name{example4}
\alias{example4}
\docType{data}
\title{Example univariate response data with outlier for MRPP
}
\description{Hypothetical univariate response data in 2 groups with 1 outlier (Mielke 1986).
}
\usage{data(example4)}
\format{
  A data frame with 30 observations on the following 2 variables.
  \describe{
    \item{\code{group}}{Grouping variable (1 or 2).}
    \item{\code{response}}{Unvariate response variable.}
  }
}

\source{
Mielke, P.W. 1986. Non-metric statistical analyses: some metric alternatives. \emph{Journal of Statistical Planning and Inference} \bold{13}, 377--387.
}

\examples{

par(mfrow = c(2,1))

hist(example4$response[example4$group==1],breaks = 6,xlim = c(12,17),
  ylim = c(0,7),ylab = "Count",xlab = "",main = "Group 1",col = "blue")

hist(example4$response[example4$group==2],breaks = 25,xlim = c(12,17),
 ylim = c(0,7),ylab = "Count",xlab = "Response",main = "Group 2",col = "blue")

}
\keyword{datasets}

