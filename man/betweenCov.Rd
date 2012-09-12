\name{betweenCov}
\alias{betweenCov}
\title{
  Between-class Covariance Matrix
}
\description{
  Calculates between-class covariance matrix 
}
\usage{
betweenCov(variables, group, div_by_n = FALSE)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory variables (No missing values are allowed)}
  \item{group}{vector or factor with group memberships (No missing values are allowed)}
  \item{div_by_n}{logical indicating division by number of observations}
}
\details{
  When \code{div_by_n=TRUE} the covariance matrices are divided by n (number of observations), otherwise they are divided by n-1
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{getWithin}}, \code{\link{betweenSS}}, \code{\link{withinCov}}, \code{\link{totalCov}}
}
\examples{
  \dontrun{
  # load iris dataset
  data(iris)
  
  # between-class covariance matrix (dividing by n-1)
  betweenCov(iris[,1:4], iris[,5])

  # between-class covariance matrix (dividing by n)
  betweenCov(iris[,1:4], iris[,5], div_by_n=TRUE)
  }
}

