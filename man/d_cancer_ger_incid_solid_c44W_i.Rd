\name{d_cancer_ger_incid_solid_c44W_i}
\alias{d_cancer_ger_incid_solid_c44W_i}
\docType{data}
\title{Solid cancer incidence rates Germany 2023 including C44}
\description{
Solid cancer (ICD10 C00-C80) incidence rates in Germany in 2023, including C44, interpolated to yearly attained age, per 1 PY. Data frame in wide format with respect to sex.}
\usage{data(d_cancer_ger_incid_solid_c44W_i)}
\format{
  Data frame with 3 variables.
  \describe{
    \item{\code{age_n}}{Age from 0 to 100 in steps of 1 year.}
    \item{\code{rate_f}}{Incidence rate per 1 PY for women.}
    \item{\code{rate_m}}{Incidence rate per 1 PY for men.}
  }
}
\details{
Data source: Robert Koch Institute (RKI) \url{https://www.krebsdaten.de/}
}
\seealso{
\code{\link{interpol_cancer_rate}},
\code{\link{get_lear_1}},
\code{\link{get_lear_pop}},
\code{\link{d_cancer_ger_incid_solidW_i}},
\code{\link{d_cancer_ger_incid_breastW_i}},
\code{\link{d_cancer_ger_incid_leuk_lymphW_i}},
\code{\link{d_cancer_ger_mort_solidW_i}},
\code{\link{d_cancer_ger_mort_breastW_i}},
\code{\link{d_cancer_ger_mort_leuk_lymphW_i}}
}
\examples{
head(d_cancer_ger_incid_solid_c44W_i)
}
\keyword{datasets}
