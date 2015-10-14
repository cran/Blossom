\name{npof}
\alias{npof}
\docType{data}
\title{
Orientation of movements of newt
}
\description{An analysis of the orientation of movements of striped
newts \emph{Notophtalmus peristriatus} immigrating to and emigrating
from Breezeway Pond, Florida in 1985 - 1990 (Dodd and Cade 1998) 
including the angular orientation of 585 females immigrating to
and 564 emigrating from the pond that were captured in pitfall
buckets inside and outside of a drift fence surrounding the pond.

}
\usage{data(npof)}
\format{
  A data frame with 1149 observations on the following 2 variables.
  \describe{
    \item{\code{angle}}{The angular orientation of each female newt.}
    \item{\code{ei}}{This grouping variable has 1's for emigrating and 2's for immigrating females.}
  }
}

\source{
Dodd, C.K., and B.S. Cade. 1998. Movement patterns and the conservation of amphibians breeding in small, temporary wetlands. \emph{Conservation Biology} \bold{12}, 331--339.
}

\examples{

str(npof)
npof$ei <- factor(npof$ei,levels=c(1,2),labels=c("Immigrating","Emigrating"))
group.counts <- as.data.frame(table(npof))

par(mfrow = c(1,2),mar = c(8,0,6,0),pty = "s")
limit <- max(group.counts$Freq)

for(i in 1:2){
    freq <- group.counts$Freq[group.counts$ei==unique(group.counts$ei)[i]]
    plot(c(-(limit+14),limit+14),
         c(-(limit+14),limit+14),type = "n",xaxt = "n",
         yaxt = "n",xlab = "",ylab = "",main =(unique(group.counts$ei)[i]))
         
    symbols(x = 0,y = 0,circles = limit+6,col = "blue",inches = FALSE,
           add = TRUE,lwd = 1.5)        
    angl <- as.numeric(as.character(
            group.counts$angle[group.counts$ei==unique(group.counts$ei)[i]]))
    segments(x0 = rep(0,times = 24),y0 = rep(0,times = 24),
         x1 = c(sin(angl*pi/180)*freq,0),y1 = c(cos(angl*pi/180)*freq,limit+4),
         col = c(rep("blue",times = 23),"black"),
         lwd = c(rep(2,times=23),1.5))
    text(x = c(limit+12,0,-limit-12,0),
         y = c(0,limit+12,0,-limit-10),labels = c("90","0","270","180"))
    text(x = rep(-7,times = 6),y = seq(from = 10,to = 70,by = 10),
        labels = seq(from = 10,to = 70,by = 10))
    segments(x0 = rep(-2,times = 6),y0 = seq(from = 10,to = 70,by = 10),
        x1 = rep(0,times = 6),y1 = seq(from = 10,to = 70,by = 10))                         
}

dev.off()
}
\keyword{datasets}
