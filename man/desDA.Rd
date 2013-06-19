\name{desDA}
\alias{desDA}
\title{Descriptive Discriminant Analysis}
\usage{
  desDA(variables, group, covar = "within")
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables}

  \item{group}{vector or factor with group memberships}

  \item{covar}{character string indicating the covariance
  matrix to be used. Options are \code{"within"} and
  \code{"total"}}
}
\value{
  An object of class \code{"desda"}, basically a list with
  the following elements

  \item{power}{table with discriminant power of the
  explanatory variables}

  \item{values}{table of eigenvalues}

  \item{discrivar}{table of discriminant variables, i.e.
  the coefficients of the linear discriminant functions}

  \item{discor}{table of correlations between the variables
  and the discriminant axes}

  \item{scores}{table of discriminant scores for each
  observation}
}
\description{
  Performs a Descriptive Discriminant Analysis (a.k.a.
  Factorial Discriminant Analysis from the french
  \emph{Analyse Factorielle Discriminante})
}
\details{
  When \code{covar="within"} the estimated pooled
  within-class covariance matrix is used in the
  calculations. \cr When \code{covar="total"} the total
  covariance matrix is used in the calculations. \cr The
  difference between \code{covar="within"} and
  \code{covar="total"} is in the obtained eigenvalues.

  The estiamted pooled within-class covariance matrix is
  actually the within-class covariance matrix divided by
  the number of observations minus the number of classes
  (see \code{\link{getWithin}})
}
\examples{
\dontrun{
  # load bordeaux wines dataset
  data(bordeaux)

  # descriptive discriminant analysis with within covariance matrix
  my_dda1 = desDA(bordeaux[,2:5], bordeaux$quality)
  my_dda1

  # descriptive discriminant analysis with total covariance matrix
  my_dda2 = desDA(bordeaux[,2:5], bordeaux$quality, covar="total")
  my_dda2

  # plot factor coordinates with ggplot
  library(ggplot2)
  bordeaux$f1 = my_dda1$scores[,1]
  bordeaux$f2 = my_dda1$scores[,2]
  ggplot(data=bordeaux, aes(x=f1, y=f2, colour=quality)) +
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_text(aes(label=year), size=4) +
  opts(title="Discriminant Map - Bordeaux Wines (years)")
 }
}
\author{
  Gaston Sanchez
}
\references{
  Lebart L., Piron M., Morineau A. (2006) \emph{Statistique
  Exploratoire Multidimensionnelle}. Dunod, Paris.
}
\seealso{
  \code{\link{discPower}}
}

