\name{fraserf}
\alias{fraserf}
\docType{data}
\title{Subalpine forest tree cover data
}
\description{Lodgepole pine and Engelmann spruce canopy cover modeled as a function of basal area and density of the trees with least absolute deviation regression.
}
\usage{data(fraserf)}
\format{
  A data frame with 31 observations on the following 11 variables.
  \describe{
    \item{\code{apico}}{Basal area (m^2/ha) of lodgepole pine adjusted for slope of terrain.}
    \item{\code{apien}}{Basal area (m^2/ha) of Engelmann spruce.}
    \item{\code{apien2}}{Basal area squared of Engelmann spruce.}
    \item{\code{lcc}}{Lodgepole pine canopy cover.}
    \item{\code{scc}}{Englemann spruce canopy cover.}
    \item{\code{picopha}}{Lodgepole pine stem density (number/ha).}
    \item{\code{pienpha}}{Englemann spruce stem density (number/ha).}
    \item{\code{z81}}{The transformation lcc-.81(apico).}
    \item{\code{z105}}{The transformation lcc-1.05(apico).}
    \item{\code{z50}}{The transformation lcc-.50(apico).}
    \item{\code{z90}}{The transformation lcc-.90(apico).}
  }
}

\source{
Cade, B.S. 1997. Comparison of tree basal area and canopy cover in habitat models: Subalpine forest. \emph{Journal of Wildlife Management} \bold{61}, 326--335.
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

pairs(fraserf[,c(1,2,4,5,6,7)],main = "Lodgepole pine data\n with Spearman correlation coefficient",
     lower.panel = panel.smooth, upper.panel = panel.cor)
}
\keyword{datasets}
