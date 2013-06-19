\name{groupVars}
\alias{groupVars}
\title{Group Variances}
\usage{
  groupVars(variables, group, na.rm = FALSE)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables (may contain missing values)}

  \item{group}{vector or factor with group memberships}

  \item{na.rm}{logical indicating whether missing values
  should be removed}
}
\value{
  matrix of group variances (with variables in the rows,
  and groups in the columns)
}
\description{
  Calculates the variances for each group
}
\examples{
\dontrun{
  # dataset iris
  data(iris)

  # group variances
  groupVars(iris[,1:4], iris[,5])
  }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{groupMeans}}, \code{\link{groupStds}},
  \code{\link{groupMedians}}, \code{\link{groupQuants}}
}

