\docType{data}
\name{bordeaux}
\alias{bordeaux}
\title{Bordeaux Wines Dataset}
\format{A data frame with 34 observations on the following 6 variables.
\tabular{ll}{ \code{year} \tab year of harvest\cr \code{temperature} \tab
sum of daily average temperatures (in celsius degrees)\cr \code{sun} \tab
duration of insolation (in hours)\cr \code{heat} \tab number of super-hot
days\cr \code{rain} \tab rain level (in millimeters)\cr \code{quality} \tab
wine quality: a factor with levels \code{bad}, \code{good}, and
\code{medium}\cr }}
\description{
  Quality measures of wines from Bordeaux, France
}
\examples{
\dontrun{
  # load data
  data(bordeaux)

  # structure of data
  str(bordeaux)
 }
}
\references{
  Chapter 10: Analyse Discriminante, page 353. \cr
  Tenenhaus M. (2007) \emph{Statistique}. Dunod, Paris.
}
\keyword{datasets}

