\name{FRatio}
\alias{FRatio}
\title{F-Statistic Ratio}
\usage{
  FRatio(variable, group)
}
\arguments{
  \item{variable}{a quantitative variable}

  \item{group}{a vector or factor with group memberships
  (i.e. qualitative variable)}
}
\value{
  F-statistic and its p-value
}
\description{
  Calcualtes the F-statistic between a quantitative
  variable and a qualitative variable
}
\examples{
\dontrun{
  # load bordeaux wines dataset
  data(bordeaux)

  # F-statistic ratio between temperature and quality
  FRatio(bordeaux$temperature, bordeaux$quality)
  }
}
\author{
  Gaston Sanchez
}
\references{
  Tenenhaus M. (2007) \emph{Statistique}. Dunod, Paris.
}
\seealso{
  \code{\link{discPower}}, \code{\link{corRatio}}
}

