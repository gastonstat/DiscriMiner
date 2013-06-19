\name{disqual}
\alias{disqual}
\title{Discriminant Analysis on Qualitative Variables}
\usage{
  disqual(variables, group, validation = NULL,
    learn = NULL, test = NULL, autosel = TRUE, prob = 0.05)
}
\arguments{
  \item{variables}{data frame with qualitative explanatory
  variables (coded as factors)}

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

  \item{autosel}{logical indicating automatic selection of
  MCA components}

  \item{prob}{probability level for automatic selection of
  MCA components. Default \code{prob = 0.05}}
}
\value{
  An object of class \code{"disqual"}, basically a list
  with the following elements:

  \item{raw_coefs}{raw coefficients of discriminant
  functions}

  \item{norm_coefs}{normalizaed coefficients of
  discriminant functions, ranging from 0 - 1000}

  \item{confusion}{confusion matrix}

  \item{scores}{discriminant scores for each observation}

  \item{classification}{assigned class}

  \item{error_rate}{misclassification error rate}
}
\description{
  Implementation of the DISQUAL methodology. Disqual
  performs a Fishers Discriminant Analysis on components
  from a Multiple Correspondence Analysis
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
  # load insurance dataset
  data(insurance)

  # disqual analysis with no validation
  my_disq1 = disqual(insurance[,-1], insurance[,1], validation=NULL)
  my_disq1

  # disqual analysis with cross-validation
  my_disq2 = disqual(insurance[,-1], insurance[,1], validation="crossval")
  my_disq2
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

  Saporta G., Niang N. (2006) Correspondence Analysis and
  Classification. In \emph{Multiple Correspondence Analysis
  and Related Methods}, Eds. Michael Greenacre and Jorg
  Blasius, 371-392. Chapman and Hall/CRC
}
\seealso{
  \code{\link{easyMCA}}, \code{\link{classify}},
  \code{\link{binarize}}
}

