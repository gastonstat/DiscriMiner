\name{classify}
\alias{classify}
\title{Classification function}
\usage{
  classify(DA_object, newdata)
}
\arguments{
  \item{DA_object}{discriminant analysis object}

  \item{newdata}{vector or matrix or data frame with
  variables for which their classes will be calculated}
}
\value{
  A list with the following elements

  \item{scores}{discriminant scores for each observation}

  \item{pred_class}{predicted class}
}
\description{
  Classify provided observations based on a given
  Discriminant object
}
\details{
  A \code{DA_object} is a discriminant analysis (DA) object
  obtained from a geometric predictive DA (class
  \code{"geoda"}), a linear DA (class \code{"linda"}), a
  quadratic DA (class \code{"quada"}), or a DISQUAL
  analysis (class \code{"disqual"})
}
\examples{
\dontrun{
  # load iris dataset
  data(iris)

  # linear discriminant analysis
  my_lin1 = linDA(iris[,1:4], iris$Species)

  # select a sample of 15 observations
  set.seed(111)
  obs = sample(1:nrow(iris), 15)
  some_data = iris[obs, 1:4]

  # classify some_data
  get_classes = classify(my_lin1, some_data)
  get_classes

  # compare the results against original class
  table(iris$Species[obs], get_classes$pred_class)
  }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{geoDA}}, \code{\link{linDA}},
  \code{\link{quaDA}}, \code{\link{plsDA}},
  \code{\link{disqual}}
}

