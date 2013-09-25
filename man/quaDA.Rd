\name{quaDA}
\alias{quaDA}
\title{Quadratic Discriminant Analysis}
\usage{
  quaDA(variables, group, prior = NULL, validation = NULL,
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
  An object of class \code{"quada"}, basically a list with
  the following elements:

  \item{confusion}{confusion matrix}

  \item{scores}{discriminant scores for each observation}

  \item{classification}{assigned class}

  \item{error_rate}{misclassification error rate}
}
\description{
  Performs a Quadratic Discriminant Analysis
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
  # load iris dataset
  data(iris)

  # quadratic discriminant analysis with no validation
  my_qua1 = quaDA(iris[,1:4], iris$Species)
  my_qua1$confusion
  my_qua1$error_rate

  # quadratic discriminant analysis with cross-validation
  my_qua2 = quaDA(iris[,1:4], iris$Species, validation="crossval")
  my_qua2$confusion
  my_qua2$error_rate

  # quadratic discriminant analysis with learn-test validation
  learning = c(1:40, 51:90, 101:140)
  testing = c(41:50, 91:100, 141:150)
  my_qua3 = quaDA(iris[,1:4], iris$Species, validation="learntest",
      learn=learning, test=testing)
  my_qua3$confusion
  my_qua3$error_rate
  }
}
\author{
  Gaston Sanchez
}
\references{
  Lebart L., Piron M., Morineau A. (2006) \emph{Statistique
  Exploratoire Multidimensionnelle}. Dunod, Paris.

  Tenenhaus G. (2007) \emph{Statistique}. Dunod, Paris.

  Tuffery S. (2011) \emph{Data Mining and Statistics for
  Decision Making}. Wiley, Chichester.
}
\seealso{
  \code{\link{classify}}, \code{\link{desDA}},
  \code{\link{geoDA}}, \code{\link{linDA}},
  \code{\link{plsDA}}
}

