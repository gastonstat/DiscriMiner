\name{corRatio}
\alias{corRatio}
\title{Correlation Ratio}
\usage{
  corRatio(variable, group)
}
\arguments{
  \item{variable}{a single quantitative variable}

  \item{group}{vector or factor with group memberships
  (qualitative variable)}
}
\description{
  Calculates the correlation ratio between a quantitaive
  variable and a qualitative variable
}
\details{
  No missing values are allowed
}
\examples{
\dontrun{
  # iris dataset
  data(iris)

  # correlation ratio between Petal-Length and Species
  corRatio(iris$Petal.Length, iris$Species)
  }
}
\author{
  Gaston Sanchez
}
\references{
  Tenenhaus, M. (2007) \emph{Statistique}. Dunod, Paris.
}
\seealso{
  \code{\link{FRatio}}, \code{\link{discPower}}
}

