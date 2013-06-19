\name{betweenSS}
\alias{betweenSS}
\title{Between-class Sum of Squares Matrix}
\usage{
  betweenSS(variables, group)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables (No missing values are allowed)}

  \item{group}{vector or factor with group membership (No
  missing values are allowed)}
}
\description{
  Calculates between-class sum of squares and cross product
  matrix (a.k.a. between-class scatter matrix)
}
\examples{
\dontrun{
  # load iris dataset
  data(iris)

  # between-class scatter matrix
  betweenSS(iris[,1:4], iris[,5])
  }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{betweenCov}}, \code{\link{withinSS}},
  \code{\link{totalSS}}
}

