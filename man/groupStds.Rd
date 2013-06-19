\name{groupStds}
\alias{groupStds}
\title{Group Standard Deviations}
\usage{
  groupStds(variables, group, na.rm = FALSE)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables (may contain missing values)}

  \item{group}{vector or factor with group memberships}

  \item{na.rm}{logical indicating whether missing values
  should be removed}
}
\value{
  matrix of group standard deviations (with variables in
  the rows, and groups in the columns)
}
\description{
  Calculates the standard deviations for each group
}
\examples{
\dontrun{
  # dataset iris
  data(iris)

  # group standard deviations
  groupStds(iris[,1:4], iris[,5])
  }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{groupMeans}}, \code{\link{groupVars}},
  \code{\link{groupMedians}}, \code{\link{groupQuants}}
}

