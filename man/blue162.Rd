\name{blue162}
\alias{blue162}
\docType{data}
\title{
Example data of biweekly grouse locations
}
\description{Sequential locations of a blue grouse on its winter ranges in Middle Park, Colorado.
}
\usage{data(blue162)}
\format{
  A data frame with 12 observations on the following 5 variables.
  \describe{
    \item{\code{date}}{Year, month, and day of location.}
    \item{\code{bird}}{Bird identification number.}
    \item{\code{sexage}}{Sex = 2 = female, age = 3 = adult.}
    \item{\code{lat}}{Latitudinal UTM coordinate.}
    \item{\code{long}}{Longitudinal UTM coordinate.}
  }
}

\source{
Cade, B.S., and R.W. Hoffman. 1993. Differential migration of blue grouse in Colorado. \emph{Auk} \bold{110}, 70--77.
}

\examples{
str(blue162)

 seq.cols <- topo.colors(nrow(blue162))
 
 #I jitter locations so overlapping points can be seen
plot(jitter(blue162$long,amount = 4),jitter(blue162$lat,amount = 4),
 bg = seq.cols,cex = 2,pch = 21,xlab = "Longitude",ylab = "Latitude",
 main = "Spatial locations of bluegrouse \nshowing temporal ordering of observations")
 
for(i in 1:length(seq.cols)){
    rect(max(blue162$long)-10,min(blue162$lat)+10*(i-1),
    max(blue162$long),min(blue162$lat)+10*(i),col = seq.cols[i],lty="blank")
    text(max(blue162$long)-15,min(blue162$lat)+10*(i-1)+5,label=i)
    }
    
text(max(blue162$long)-20,min(blue162$lat)+130,
  label = "time order of \nobservations")

}
\keyword{datasets}
