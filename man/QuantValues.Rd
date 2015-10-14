\name{QuantValues-methods}
\docType{methods}
\alias{QuantValues-methods}
\alias{QuantValues}
\alias{QuantValues,LADObj-method}
\title{Report quantile values}
\description{Reports all quantile regression estimates if \code{all.quants} was set to \code{TRUE}.

}
\usage{
QuantValues(x)
}
\arguments{
  \item{x}{An object of a suitable class - at present only
    \code{"\linkS4class{LADObj}"}.}
}
\value{
  A \code{matrix} of all possible quantile regression estimates.
}
\examples{
Out <- lad(lnlctm~widrat,data = lahontan,all.quants = TRUE)
summary(Out)
AllQuants <- QuantValues(Out)
}
\keyword{methods}
