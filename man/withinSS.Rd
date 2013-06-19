\name{withinSS}
\alias{withinSS}
\title{Within-class Sum of Squares Matrix}
\usage{
  withinSS(variables, group)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables (No missing values are allowed)}

  \item{group}{vector or factor with group memberships (No
  missing values are allowed)}
}
\description{
  Calculates within-class sum of squares and cross product
  matrix (a.k.a. within-class scatter matrix)
}
\examples{
\dontrun{
  # load iris dataset
  data(iris)

  # within-class scatter matrix
  withinSS(iris[,1:4], iris[,5])
  }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{withinCov}}, \code{\link{betweenSS}},
  \code{\link{totalSS}}
}

