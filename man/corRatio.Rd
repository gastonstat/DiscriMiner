\name{corRatio}
\alias{corRatio}
\title{
  Correlation Ratio
}
\description{
  Calculates the correlation ratio between a quantitaive variable and a qualitative variable
}
\usage{
corRatio(variable, group)
}
\arguments{
  \item{variable}{a single quantitative variable}
  \item{group}{vector or factor with group memberships (qualitative variable)}
}
\details{
  No missing values are allowed
}
\references{
  Tenenhaus, M. (2007) \emph{Statistique}. Dunod, Paris.
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{FRatio}}, \code{\link{discPower}}
}
\examples{
  \dontrun{
  # iris dataset 
  data(iris)
  
  # correlation ratio between Petal-Length and Species
  corRatio(iris$Petal.Length, iris$Species)
  }
}

