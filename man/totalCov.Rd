\name{totalCov}
\alias{totalCov}
\title{
  Total Covariance Matrix
}
\description{
  Calculates total covariance matrix
}
\usage{
totalCov(variables, div_by_n = FALSE)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory variables (No missing values are allowed)}
  \item{div_by_n}{logical indicating division by number of observations}
}
\details{
  When \code{div_by_n=TRUE} the covariance matrices are divided by n (number of observations), otherwise they are divided by n-1
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{totalSS}}, \code{\link{betweenCov}}, \code{\link{withinCov}}
}
\examples{
  \dontrun{
  # load iris dataset
  data(iris)
  
  # total covariance matrix (dividing by n-1)
  totalCov(iris[,1:4])

  # total covariance matrix (dividing by n)
  totalCov(iris[,1:4], div_by_n=TRUE)
  }
}

