\name{withinCov}
\alias{withinCov}
\title{Within-class Covariance Matrix}
\usage{
  withinCov(variables, group, div_by_n = FALSE)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables (No missing values are allowed)}

  \item{group}{vector or factor with group memberships (No
  missing values are allowed)}

  \item{div_by_n}{logical indicating division by number of
  observations}
}
\description{
  Calculates the within-class covariance matrix
}
\details{
  When \code{div_by_n=TRUE} the covariance matrices are
  divided by n (number of observations), otherwise they are
  divided by n-1
}
\examples{
\dontrun{
  # load iris dataset
  data(iris)

  # within-class covariance matrix (dividing by n-1)
  withinCov(iris[,1:4], iris[,5])

  # within-class covariance matrix (dividing by n)
  withinCov(iris[,1:4], iris[,5], div_by_n=TRUE)
  }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{withinSS}}, \code{\link{betweenCov}},
  \code{\link{totalCov}}
}

