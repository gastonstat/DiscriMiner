\name{plsDA}
\alias{plsDA}
\title{PLS Discriminant Analysis}
\usage{
  plsDA(variables, group, autosel = TRUE, comps = 2,
    validation = NULL, learn = NULL, test = NULL,
    cv = "LOO", k = NULL, retain.models = FALSE)
}
\arguments{
  \item{variables}{matrix or data frame with explanatory
  variables}

  \item{group}{vector or factor with group memberships}

  \item{autosel}{logical indicating automatic selection of
  PLS components by cross-validation. Default
  \code{autosel=TRUE}}

  \item{comps}{integer greater than one indicating the
  number of PLS components to retain. Used only when
  \code{autosel=FALSE}}

  \item{validation}{type of validation, either \code{NULL}
  or \code{"learntest"}. Default \code{NULL}}

  \item{learn}{optional vector of indices for a learn-set.
  Only used when \code{validation="learntest"}. Default
  \code{NULL}}

  \item{test}{optional vector of indices for a test-set.
  Only used when \code{validation="learntest"}. Default
  \code{NULL}}

  \item{cv}{string indicating the type of crossvalidation.
  Avialable options are \code{"LOO"} (Leave-One-Out) and
  \code{"LKO"} (Leave-K fold-Out)}

  \item{k}{fold left out if using LKO (usually 7 or 10)}

  \item{retain.models}{whether to retain lower models (i.e.
  all lower component results)}
}
\value{
  An object of class \code{"plsda"}, basically a list with
  the following elements:

  \item{functions}{table with discriminant functions}

  \item{confusion}{confusion matrix}

  \item{scores}{discriminant scores for each observation}

  \item{loadings}{loadings}

  \item{y.loadings}{y loadings}

  \item{classification}{assigned class}

  \item{error_rate}{misclassification error rate}

  \item{components}{PLS components}

  \item{Q2}{quality of loo cross-validation}

  \item{R2}{R-squared coefficients}

  \item{VIP}{Variable Importance for Projection}

  \item{comp_vars}{correlations between components and
  variables}

  \item{comp_group}{correlations between components and
  groups}
}
\description{
  Performs a Partial Least Squares (PLS) Discriminant
  Analysis by giving the option to include a random leave-k
  fold out cross validation
}
\details{
  When \code{validation=NULL} leave-one-out (loo)
  cross-validation is performed. \cr When
  \code{validation="learntest"} validation is performed by
  providing a learn-set and a test-set of observations. \cr
}
\examples{
\dontrun{
# load iris dataset
data(iris)

# PLS discriminant analysis specifying number of components = 2
my_pls1 = plsDA(iris[,1:4], iris$Species, autosel=FALSE, comps=2)
my_pls1$confusion
my_pls1$error_rate
# plot circle of correlations
plot(my_pls1)

# PLS discriminant analysis with automatic selection of components
my_pls2 = plsDA(iris[,1:4], iris$Species, autosel=TRUE)
my_pls2$confusion
my_pls2$error_rate

# linear discriminant analysis with learn-test validation
learning = c(1:40, 51:90, 101:140)
testing = c(41:50, 91:100, 141:150)
my_pls3 = plsDA(iris[,1:4], iris$Species, validation="learntest",
learn=learning, test=testing)
my_pls3$confusion
my_pls3$error_rate
}
}
\author{
  Charles Determan Jr, Gaston Sanchez
}
\references{
  Tenenhaus M. (1998) \emph{La Regression PLS}. Editions
  Technip, Paris.

  Perez-Enciso M., Tenenhaus M. (2003) \emph{Prediction of
  clinical outcome with microarray data: a partial least
  squares discriminant analysis (PLS-DA) approach}. Human
  Genetics 112: 581-592.
}
\seealso{
  \code{\link{classify}}, \code{\link{geoDA}},
  \code{\link{linDA}}, \code{\link{quaDA}}
}

