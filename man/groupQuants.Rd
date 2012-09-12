\name{groupQuants}
\alias{groupQuants}
\title{
  Group Quantiles
}
\description{
  Calculates the specified quantiles for each group
}
\usage{
groupQuants(variables, group, prob, na.rm = FALSE)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory variables (may contain missing values)}
  \item{group}{vector or factor with group memberships}
  \item{prob}{probability value (numeric value between 0 and 1)}
  \item{na.rm}{logical indicating whether missing values should be removed}
}
\value{
  matrix of group quantiles (with variables in the rows, and groups in the columns)
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{groupMeans}}, \code{\link{groupVars}}, \code{\link{groupStds}}, \code{\link{groupMedians}}
}
\examples{
  \dontrun{
  # dataset iris
  data(iris)

  # group quantile prob=20
  groupQuants(iris[,1:4], iris[,5], prob=0.20)
  }
}
