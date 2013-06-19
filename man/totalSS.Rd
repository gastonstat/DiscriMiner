\name{totalSS}
\alias{totalSS}
\title{Total Sum of Squares Matrix}
\usage{
  totalSS(variables)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables}
}
\description{
  Calculates the total sum of squares and cross product
  matrix (a.k.a. total scatter matrix)
}
\examples{
\dontrun{
  # load iris dataset
  data(iris)

  # total scatter matrix
  totalSS(iris[,1:4])
  }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{totalCov}}, \code{\link{betweenSS}},
  \code{\link{withinSS}}
}

