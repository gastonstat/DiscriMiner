\name{totalSS}
\alias{totalSS}
\title{
  Total Sum of Squares Matrix
}
\description{
  Calculates the total sum of squares and cross product matrix (a.k.a. total scatter matrix)
}
\usage{
totalSS(variables)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory variables}
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{totalCov}}, \code{\link{betweenSS}}, \code{\link{withinSS}}
}
\examples{
  \dontrun{
  # load iris dataset
  data(iris)
  
  # total scatter matrix
  totalSS(iris[,1:4])
  }
}

