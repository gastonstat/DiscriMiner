\docType{data}
\name{infarctus}
\alias{infarctus}
\title{Infarctus dataset}
\format{A data frame with 101 observations on the following 8 variables.
\tabular{ll}{ \code{FRCAR} \tab Frequence Cardiaque (i.e. heart rate)\cr
\code{INCAR} \tab Index Cardique (cardiac index)\cr \code{INSYS} \tab Index
Systolique (systolic index)\cr \code{PRDIA} \tab Pression Diastolique
(diastolic pressure)\cr \code{PAPUL} \tab Pression Arterielle Pulmonaire
(pulmonary artery pressure)\cr \code{PVENT} \tab Pression Ventriculaire
(ventricular pressure)\cr \code{REPUL} \tab Resistance Pulmonaire (pulmonary
resistance)\cr \code{PRONO} \tab Pronostic (prognosis): a factor with levels
\code{dead} and \code{survive}\cr }}
\description{
  Infarctus dataset from Saporta (2006)
}
\examples{
\dontrun{
  # load data
  data(infarctus)

  # summary of variables
  summary(infarctus)
 }
}
\references{
  Chapter 18: Analyse discriminante et regression
  logistique, pp 453-454 \cr Saporta G. (2006)
  \emph{Probabilites, analyse des donnees et statistique}.
  Editions Technip, Paris.
}
\keyword{datasets}

