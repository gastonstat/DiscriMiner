\name{binarize}
\alias{binarize}
\title{
  Binarize a data frame into a super-indicator matrix
}
\description{
  Convert a data frame with factors into a super-indicator matrix (a.k.a. complete disjunctive table from the french \emph{tableau disjonctive complete})
}
\usage{
  binarize(variables)
}
\arguments{
  \item{variables}{data frame with categorical variables (coded as factors)}
}
\value{
  A super-indicator matrix of binary data
}
\references{
  Lebart L., Piron M., Morineau A. (2006) \emph{Statistique Exploratoire Multidimensionnelle}. Dunod, Paris.
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{easyMCA}}
}
\examples{
  \dontrun{
  # load insurance cars dataset
  data(insurance)

  # super-indicator matrix of binary data
  bin_insure = binarize(insurance[,-1])
  head(bin_insure)
  }
}

