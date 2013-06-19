\name{easyMCA}
\alias{easyMCA}
\title{Multiple Correspondence Analysis}
\usage{
  easyMCA(variables)
}
\arguments{
  \item{variables}{data frame with categorical variables
  (coded as factors)}
}
\value{
  An object of class \code{"qualmca"}, basically a list
  with the following elements:

  \item{values}{table with eigenvalues}

  \item{coefficients}{coefficients of factorial axes}

  \item{components}{factor coordinates}
}
\description{
  Performs a basic Multiple Correspondence Analysis (MCA)
}
\examples{
\dontrun{
  # load insurance wines dataset
  data(insurance)

  # multiple correspondence analysis
  mca1 = easyMCA(insurance[,-1])
  mca1
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
}
\seealso{
  \code{\link{disqual}}, \code{\link{binarize}}
}

