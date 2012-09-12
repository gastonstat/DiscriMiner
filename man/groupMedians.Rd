\name{groupMedians}
\alias{groupMedians}
\title{
  Group Medians
}
\description{
  Calculates the medians for each group
}
\usage{
groupMedians(variables, group, na.rm = FALSE)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory variables (may contain missing values)}
  \item{group}{vector or factor with group memberships}
  \item{na.rm}{logical indicating whether missing values should be removed}
}
\value{
  matrix of group medians (with variables in the rows, and groups in the columns)
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{groupVars}}, \code{\link{groupStds}}, \code{\link{groupMeans}}, \code{\link{groupQuants}}
}
\examples{
  \dontrun{
  # dataset iris
  data(iris)

  # group means
  groupMedians(iris[,1:4], iris[,5])
  }
}

