\name{getWithin}
\alias{getWithin}
\title{Within-class Covariance Matrix}
\usage{
  getWithin(variables, group)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables (No missing values are allowed)}

  \item{group}{vector or factor with group memberships (No
  missing values are allowed)}
}
\description{
  Calculates the estimated within-class covariance matrix
}
\details{
  The obtained matrix is the estimated within-class
  covariance matrix (i.e. within-class covariance matrix
  divided by its degrees of freedom \code{n-k}, where
  \code{n} is the number of observations and \code{k} is
  the number of groups)
}
\examples{
\dontrun{
  # load iris dataset
  data(iris)

  # estimated within-class covariance matrix (dividing by n-k)
  getWithin(iris[,1:4], iris[,5])

  # compared to the within-class covariance matrix (dividing by n-1)
  withinCov(iris[,1:4], iris[,5])
  }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{withinCov}}
}

