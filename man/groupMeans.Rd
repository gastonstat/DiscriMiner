\name{groupMeans}
\alias{groupMeans}
\title{Group Means}
\usage{
  groupMeans(variables, group, na.rm = FALSE)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables (may contain missing values)}

  \item{group}{vector or factor with group memberships}

  \item{na.rm}{logical indicating whether missing values
  should be removed}
}
\value{
  matrix of group means (with variables in the rows, and
  groups in the columns)
}
\description{
  Calculates means for each group
}
\examples{
\dontrun{
  # dataset iris
  data(iris)

  # group means
  groupMeans(iris[,1:4], iris[,5])
  }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{groupVars}}, \code{\link{groupStds}},
  \code{\link{groupMedians}}, \code{\link{groupQuants}}
}

