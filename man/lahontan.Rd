\name{lahontan}
\alias{lahontan}
\docType{data}
\title{
Lahontan cutthroat trout density in streams
}
\description{
Lahontan cutthroat trout (\emph{Oncorhynchus clarki henshawi}) numbers per meter of stream to stream width:depth ratio for n = 71 observations of streams across years in Nevada (Dunham et al. 2002, Cade 2005, Cade and Richards 2006, Cade et al. 2006).
}

\usage{data(lahontan)}
\format{
  A data frame with 71 observations on the following 6 variables.
  \describe{
    \item{\code{lctm}}{Lahonton cutthroat trout per meter of stream.}
    \item{\code{widrat}}{Stream width:depth ratio}
    \item{\code{lnlctm}}{Log transformation of \code{lctm}.}
    \item{\code{wt}}{Weights estimated to account for heterogeneity.}
    \item{\code{wtwidrat}}{\code{wt} multiplied by widrat.}
    \item{\code{wtlnlctm}}{\code{wt} multiplied by lnlctm.}
  }
}

\source{
Dunham, J.B., B.S. Cade, and J.W. Terrell. 2002. Influences of spatial
and temporal variation on fish-habitat relationships defined by regression quantiles.
\emph{Transactions of the American Fisheries Society} \bold{131}, 86--98.

}
\references{
Cade, B.S. 2005.\emph{Linear models: Permutation methods}. Pages 1049--1054 in B. Everitt and D. Howell, eds. Encyclopedia of Statistics in the Behavioral Science. Vol. 2. John Wiley and Sons.

Cade, B.S., J.D. Richards, and P.W. Mielke, Jr.  2006.  Rank score and permutation testing alternatives for regression quantile estimates.  \emph{Journal of Statistical Computation and Simulation} \bold{76}, 331--355. 

Cade, B.S., and J.D. Richards.  2006.  A permutation test for quantile regression.  \emph{Journal of Agricultural, Biological, and Environmental Statistics} \bold{11}, 106--126. 
}
\examples{
str(lahontan)
 plot(lahontan) # come up with a better plot when I understand the data
}
\keyword{datasets}
