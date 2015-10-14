\name{agree2}
\alias{agree2}
\docType{data}
\title{
Lodgepole pine basal area and canopy cover
}
\description{
Analysis comparing measures of the proportion of basal area to
the proportion of canopy cover of lodgepole pine (\emph{Pinus contorta})
in 31 stands of subalpine forest in Colorado (Cade 1997).

}
\usage{data(agree2)}
\format{
  A data frame with 62 observations on the following 6 variables.
 \describe{
    \item{\code{pctscc}}{The response variable for proportion Englemann spruce.}
    \item{\code{pctfcc}}{The response variable for proportion subalpine fir.}
    \item{\code{pctlcc}}{The response variable for proportion lodgepole pine.}
    \item{\code{pctacc}}{The response variable for proportion aspen.}
    \item{\code{stand}}{The grouping variable specifying the 31 sample plots.}
    \item{\code{method}}{The blocking variable specifying whether the proportion was measured either for basal area (method = 2) or canopy cover (method = 1).}
  }
}

\source{
Cade, B.S. 1997. Comparison of tree basal area and canopy cover in habitat models: Subalpine forest.  \emph{Journal of Wildlife Management} \bold{61}, 326--335.
}
\examples{
str(agree2)

panel.cor <- function(x, y, digits = 2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y,method = "spearman"))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * 3*r)
}

pairs(agree2[,1:4],pch = 21,bg = c("red","grey")[agree2$method],cex = 1.5,
upper.panel = panel.cor)
}
\keyword{datasets}
