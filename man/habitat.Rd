\name{habitat}
\alias{habitat}
\docType{data}
\title{
Blue grouse winter habitat selection data
}
\description{Example comparing used versus available blue grouse
winter habitat described by the basal area measurements of four species of trees present in stands on winter range.
}
\usage{data(habitat)}
\format{
  A data frame with 16 observations on the following 5 variables.
  \describe{
    \item{\code{use}}{An indicator of which habitat was used by blue grouse (1) and which was available (2).}
    \item{\code{dfir}}{Basal area (m^2/ha) for Douglas-fir.}
    \item{\code{junip}}{Basal area (m^2/ha)for juniper.}
    \item{\code{aspen}}{Basal area (m^2/ha)for aspen.}
    \item{\code{other}}{Basal area (m^2/ha)for other trees.}
  }
}

\source{
Cade, B.S., and R.W. Hoffman. 1990. Winter use of Douglas-fir forests by blue grouse in Colorado. \emph{Journal of Wildlife Management} \bold{54}, 471--479.
}

\examples{
Use<-c("Used","Available")
par(mfrow = c(2,2),mar = c(3,4,3,2))
boxplot(habitat$dfir~Use[habitat$use],col = "darkgreen",
    main = "Fir basal area measurement\n in used and available habitat",
    ylab = "Basal Area Measurement")
boxplot(habitat$junip~Use[habitat$use],col = "blue",
    main = "Juniper basal area measurement\n in used and available habitat",
    ylab = "Basal Area Measurement")
boxplot(habitat$aspen~Use[habitat$use],col = "orange",
    main = "Aspen basal area measurement\n in used and available habitat",
    ylab = "Basal Area Measurement")
boxplot(habitat$other~Use[habitat$use],col = "brown",
    main = "Other basal area measurement\n in used and available habitat",
    ylab = "Basal Area Measurement")

}
\keyword{datasets}
