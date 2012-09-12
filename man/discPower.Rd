\name{discPower}
\alias{discPower}
\title{
  Discriminant Power
}
\description{
  Measures Discriminant Power of explanatory variables
}
\usage{
discPower(variables, group)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory variables}
  \item{group}{vector or factor with group membership}
}
\details{
  No missing values are allowed
}
\value{
  A data frame containing the following columns
  \item{correl_ratio}{Correlation Ratios}
  \item{wilks_lambda}{Wilks Lambda}
  \item{F_statistic}{F-statistic}
  \item{p_value}{p-value of F-statistic}  
}
\references{
  Tenenhaus M. (2007) \emph{Statistique}. Dunod, Paris.
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{corRatio}}, \code{\link{FRatio}}
}
\examples{
  \dontrun{
  # bordeaux wines dataset
  data(bordeaux)

  # discriminant power
  dp = discPower(bordeaux[,2:5], bordeaux$quality)
  dp
  }
}

