\name{FRatio}
\alias{FRatio}
\title{
  F-Statistic Ratio
}
\description{
  Calcualtes the F-statistic between a quantitative variable and a qualitative variable
}
\usage{
FRatio(variable, group)
}
\arguments{
  \item{variable}{a quantitative variable}
  \item{group}{a vector or factor with group memberships (i.e. qualitative variable)}
}
\value{
  F-statistic and its p-value
}
\references{
  Tenenhaus M. (2007) \emph{Statistique}. Dunod, Paris.
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{discPower}}, \code{\link{corRatio}}
}
\examples{
  \dontrun{
  # load bordeaux wines dataset
  data(bordeaux)

  # F-statistic ratio between temperature and quality
  FRatio(bordeaux$temperature, bordeaux$quality)
  }
}

