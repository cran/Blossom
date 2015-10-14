\name{medq}
\alias{medq}

\title{Multivariate Medians and Distance Quantiles
}
\description{
The \code{medq} command calculates univariate or multivariate medians and distance quantiles either by groups specified by a grouping variable or for the entire data set being used.
Options allow you to specify quantiles to report that differ from the default quantiles.
This is generally used as descriptive statistics with MRPP analyses and describes the distributional changes detected by MRPP.
}
\usage{
medq(variables, group, data, save.test = FALSE,
  quant = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1))
}

\arguments{
  \item{variables}{a vector specifying the names of the variables to be used in the analysis.  Any names specified here must be matched by a column name in the argument supplied to \code{data}.
  This is an optional argument and assuming the first column in the \code{data.frame} supplied to
  \code{data} is the grouping variable and all others are the responses then the variables need not be explicitly specified.}
  \item{group}{the name of the grouping variable.  This should be a column name in the \code{data.frame} supplied to the \code{data} argument. This is optional as described above.}
  \item{data}{a \code{data.frame} with either the first column representing the grouping variable and all other columns being response variables or a \code{data.frame} with names matching those
  supplied in the \code{variables} and \code{group} arguments described above.}
  \item{save.test}{stores the distance between each observation and its group bivariate median these can be later extracted using the command \code{\link{Dist2mvm}}.}
  \item{quant}{a vector of quantiles for which multivariate quantiles should be calculated.}
}

\value{\code{medq} returns an object of class \code{\link{MEDQObj}}.
}

\references{
Mielke, P.W., Jr. and Berry, K.J. 2001. \emph{Permutation methods: A distance function approach}. Springer-Verlag.
}

\seealso{
\code{\link{Dist2mvm}} and \code{\link{mrpp}}
}
\examples{
out <- medq(data = bgrouse)
summary(out)

BgDist2mvm <- Dist2mvm(out)

medq(c(distance,elev),sex,data = bgrouse,quant = c(.2,.3,.4))
}

\keyword{nonparametric}
\keyword{multivariate}
\keyword{htest}
