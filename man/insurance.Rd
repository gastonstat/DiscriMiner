\docType{data}
\name{insurance}
\alias{insurance}
\title{Insurance Dataset}
\format{A data frame with 1106 observations on the following 10 variables.
\tabular{ll}{ \code{Claims} \tab Group variable. A factor with levels
\code{bad} and \code{good}\cr \code{Use} \tab Type of Use. A factor with
levels \code{private} and \code{professional}\cr \code{Type} \tab Insurance
Type. A factor with levels \code{companies}, \code{female}, and
\code{male}\cr \code{Language} \tab Language. A factor with levels
\code{flemish} and \code{french}\cr \code{BirthCohort} \tab Birth Cohort. A
factor with levels \code{BD_1890_1949}, \code{BD_1950_1973}, and
\code{BD_unknown}\cr \code{Region} \tab Geographic Region. A factor with
levels \code{Brussels} and \code{Other_regions}\cr \code{BonusMalus} \tab
Level of bonus-malus. A factor with levels \code{BM_minus} and
\code{BM_plus}\cr \code{YearSuscrip} \tab Year of Subscription. A factor
with levels \code{YS<86} and \code{YS>=86}\cr \code{Horsepower} \tab
Horsepower. A factor with levels \code{HP<=39} and \code{HP>=40}\cr
\code{YearConstruc} \tab Year of vehicle construction. A factor with levels
\code{YC_33_89} and \code{YC_90_91}\cr }}
\description{
  Dataset of car-insurance customers from Belgium in 1992
}
\details{
  Dataset for DISQUAL method
}
\examples{
\dontrun{
  # load data
  data(insurance)

  # structure
  str(insurance)
 }
}
\references{
  Saporta G., Niang N. (2006) Correspondence Analysis and
  Classification. In \emph{Multiple Correspondence Analysis
  and Related Methods}, M. Greenacre and J. Blasius, Eds.,
  pp 371-392. Chapman & Hall/CRC, Boca Raton, Florida, USA.
}
\seealso{
  \code{\link{disqual}}
}
\keyword{datasets}

