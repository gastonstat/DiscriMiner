\name{linDA}
\alias{linDA}
\title{Linear Discriminant Analysis}
\usage{
  linDA(variables, group, prior = NULL, validation = NULL,
    learn = NULL, test = NULL, prob = FALSE)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables}

  \item{group}{vector or factor with group memberships}

  \item{prior}{optional vector of prior probabilities.
  Default \code{prior=NULL} implies group proportions}

  \item{validation}{type of validation, either
  \code{"crossval"} or \code{"learntest"}. Default
  \code{NULL}}

  \item{learn}{optional vector of indices for a learn-set.
  Only used when \code{validation="learntest"}. Default
  \code{NULL}}

  \item{test}{optional vector of indices for a test-set.
  Only used when \code{validation="learntest"}. Default
  \code{NULL}}

  \item{prob}{logical indicating whether the group
  classification results should be expressed in probability
  terms}
}
\value{
  An object of class \code{"linda"}, basically a list with
  the following elements:

  \item{functions}{table with discriminant functions}

  \item{confusion}{confusion matrix}

  \item{scores}{discriminant scores for each observation}

  \item{classification}{assigned class}

  \item{error_rate}{misclassification error rate}
}
\description{
  Performs a Linear Discriminant Analysis
}
\details{
  When \code{validation=NULL} there is no validation \cr
  When \code{validation="crossval"} cross-validation is
  performed by randomly separating the observations in ten
  groups. \cr When \code{validation="learntest"} validation
  is performed by providing a learn-set and a test-set of
  observations. \cr
}
\examples{
\dontrun{
  # load iris dataset
  data(iris)

  # linear discriminant analysis with no validation
  my_lin1 = linDA(iris[,1:4], iris$Species)
  my_lin1$confusion
  my_lin1$error_rate

  # linear discriminant analysis with cross-validation
  my_lin2 = linDA(iris[,1:4], iris$Species, validation="crossval")
  my_lin2$confusion
  my_lin2$error_rate

  # linear discriminant analysis with learn-test validation
  learning = c(1:40, 51:90, 101:140)
  testing = c(41:50, 91:100, 141:150)
  my_lin3 = linDA(iris[,1:4], iris$Species, validation="learntest", learn=learning, test=testing)
  my_lin3$confusion
  my_lin3$error_rate
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
  \code{\link{geoDA}}, \code{\link{quaDA}},
  \code{\link{plsDA}}
}

