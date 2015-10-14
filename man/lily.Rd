\name{lily}
\alias{lily}
\docType{data}
\title{
Lily seedling counts
}
\description{
Glacier lily (\emph{Erythronium grandiflorum}) seedlings as a
function of number of flowers and an index of rockiness in n = 256
contiguous 2 x 2 m quadrats
}

\usage{data(lily)}
\format{
  A data frame with 256 observations on the following 3 variables.
  \describe{
    \item{\code{seedlings}}{The number of glacier lily seedlings in a given 2 x 2 m quadrat.}
    \item{\code{flowers}}{Number of flowers in the 2 x 2 m quadrat.}
    \item{\code{rockiness}}{The rockiness index (a number between 1 and 256) for the 2 x 2 m quadrat.}
  }
}

\source{
Cade, B.S., Terrell, J.W. and Schroeder, R.L.. 1999. Estimating effects of limiting factors with regression quantiles. \emph{Ecology} \bold{80}, 311--323.
}


\examples{

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y,method = "spearman"))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(lily, lower.panel = panel.smooth, upper.panel = panel.cor,col = "grey38",
    pch = 19,lwd = 2,
    main = "Glacier lily seedlings relationship to \nnumber of flowers and rockiness")
}
\keyword{datasets}
