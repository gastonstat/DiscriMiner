\name{my_plsDA}
\alias{my_plsDA}
\title{PLS Discriminant Analysis}
\usage{
  my_plsDA(X, y, learn, test, autosel, comps, cv = "LOO",
    k = NA, retain.models = FALSE)
}
\arguments{
  \item{X}{matrix or data.frame with explanatory variables}

  \item{y}{vector or factor with group membership}

  \item{learn}{vector of learning observations}

  \item{test}{vector of testing observations}

  \item{autosel}{logical indicating automatic selection of
  PLS comps}

  \item{comps}{number of PLS components (only when
  autosel=FALSE)}

  \item{cv}{cross validation method. Options are
  \code{"LOO"} (Leave-One-Out) and \code{"LKO"} (Leave-K
  fold-Out)}

  \item{k}{fold left out if using LKO}

  \item{retain.models}{whether to retain lower models (i.e.
  all lower component results)}
}
\description{
  Perform a PLS discriminant analysis
}
\keyword{internal}

