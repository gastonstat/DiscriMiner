\name{geoDA}
\alias{geoDA}
\title{Geometric Predictive Discriminant Analysis}
\usage{
  geoDA(variables, group, validation = NULL, learn = NULL,
    test = NULL)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables}

  \item{group}{vector or factor with group memberships}

  \item{validation}{type of validation, either
  \code{"crossval"} or \code{"learntest"}. Default
  \code{NULL}}

  \item{learn}{optional vector of indices for a learn-set.
  Only used when \code{validation="learntest"}. Default
  \code{NULL}}

  \item{test}{optional vector of indices for a test-set.
  Only used when \code{validation="learntest"}. Default
  \code{NULL}}
}
\value{
  An object of class \code{"geoda"}, basically a list with
  the following elements:

  \item{functions}{table with discriminant functions}

  \item{confusion}{confusion matrix}

  \item{scores}{discriminant scores for each observation}

  \item{classification}{assigned class}

  \item{error_rate}{misclassification error rate}
}
\description{
  Performs a Geometric Predictive Discriminant Analysis
}
\details{
  When \code{validation=NULL} there is no validation \cr
  When \code{validation="crossval"} cross-validation is
  performed by randomly separating the observations in ten
  groups. \cr When \code{validation="learntest"}
  validationi is performed by providing a learn-set and a
  test-set of observations. \cr
}
\examples{
\dontrun{
  # load bordeaux wines dataset
  data(iris)

  # geometric predictive discriminant analysis with no validation
  my_geo1 = geoDA(iris[,1:4], iris$Species)
  my_geo1$confusion
  my_geo1$error_rate

  # geometric predictive discriminant analysis with cross-validation
  my_geo2 = geoDA(iris[,1:4], iris$Species, validation="crossval")
  my_geo2$confusion
  my_geo2$error_rate
  }
}
\author{
  Gaston Sanchez
}
\references{
  Lebart L., Piron M., Morineau A. (2006) \emph{Statistique
  Exploratoire Multidimensionnelle}. Dunod, Paris.

  Saporta G. (2006) \emph{Probabilites, analyse des donnees
  et statistique}. Editions Technip, Paris.

  Tuffery S. (2011) \emph{Data Mining and Statistics for
  Decision Making}. Wiley, Chichester.
}
\seealso{
  \code{\link{classify}}, \code{\link{desDA}},
  \code{\link{linDA}}, \code{\link{quaDA}},
  \code{\link{plsDA}}
}

