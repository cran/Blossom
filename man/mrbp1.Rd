\name{mrbp1}
\alias{mrbp1}
\docType{data}
\title{
Variation in oven-dried biomass of shrubs
}
\description{The data (Mielke and Iyer 1982) are from a mine
reclamation study comparing oven-dried biomass (gm) of 3 species
of shrubs in 6 treatments and 3 blocks (different plots).  
}
\usage{data(mrbp1)}
\format{
  A data frame with 18 observations on the following 5 variables.
  \describe{
    \item{\code{trtmt}}{an indicator of the treatment group:
    1 = no fertilizer, 2 = low fertilizer, 3 = high fertilizer,
    4 = mulch and no fertilizer, 5 = mulch and low fertilizer,
    and 6 = mulch and high fertilizer.}
    \item{\code{block}}{The blocking indicator for the three plots.}
    \item{\code{spp1}}{Dried biomass for species 1.}
    \item{\code{spp2}}{Dried biomass for species 2.}
    \item{\code{spp3}}{Dried biomass for species 3.}
  }
}

\source{
Mielke, P.W. and H.K. Iyer. 1982. Permutation techniques for analyzing multi-response data from randomized block experiments. \emph{Communications in Statistics - Theory and Methods} \bold{11}, 1427--1437.
}

\examples{
str(mrbp1)

par(mfrow = c(2,1),mar = c(4,4,2,2))
plot(rep(mrbp1$trtmt,times = 3)+rep(c(-.3,0,.3),each = 18),c(mrbp1$spp1,mrbp1$spp2,
    mrbp1$spp3),col = rep(c("blue","red","black"),each = nrow(mrbp1)),
    pch = rep(c(19,16,17),each = nrow(mrbp1)),
    xlab = "Treatment Group Indicator",cex = 1.5,ylab = "Dried Biomass",
    main = "Dried Biomass for each species and treatment group")
    
abline(v = seq(from = 1.5,to = 5.5,by = 1),lty = "dashed",col = "grey")
legend(x = .65, y = 23, legend = c("Species 1","Species 2","Species 3"), 
    col = c("blue","red","black"),
    pch = c(19,16,17),bg = "white")

par(mar = c(5,4,3,2))
plot(rep(mrbp1$block,times = 3)+rep(c(-.3,0,.3),each = 18),
    c(mrbp1$spp1,mrbp1$spp2,mrbp1$spp3),col = rep(c("blue","red","black"),
    each = nrow(mrbp1)),pch = rep(c(19,16,17),each = nrow(mrbp1)),
    xlab = "Block Indicator",cex = 1.5,ylab = "Dried Biomass",
    main = "Dried Biomass for each species and block",xaxp = c(1,3,2))
    
abline(v = seq(from = 1.5,to = 5.5,by = 1),lty = "dashed",col = "grey")
legend(x = .8, y = 24, legend = c("Species 1","Species 2","Species 3"), 
     col = c("blue","red","black"),
     pch = c(19,16,17),bg = "white")
}
\keyword{datasets}
