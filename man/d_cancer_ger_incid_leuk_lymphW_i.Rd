\name{d_cancer_ger_incid_leuk_lymphW_i}
\alias{d_cancer_ger_incid_leuk_lymphW_i}
\docType{data}
\title{Leukemia and lymphoma incidence rates Germany 2023}
\description{
Leukemia and lymphoma (ICD10 C91-C95) incidence rates in Germany in 2023, interpolated to yearly attained age, per 1 PY. Data frame in wide format with respect to sex. Note: Includes CLL (C91.1, C91.4) and ATL (C91.5) which are typically excluded in radiation risk models.}
\usage{data(d_cancer_ger_incid_leuk_lymphW_i)}
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
\code{\link{get_lear1}},
\code{\link{get_lear_pop}},
\code{\link{d_cancer_ger_incid_solidW_i}},
\code{\link{d_cancer_ger_incid_solid_c44W_i}},
\code{\link{d_cancer_ger_incid_breastW_i}},
\code{\link{d_cancer_ger_mort_solidW_i}},
\code{\link{d_cancer_ger_mort_breastW_i}},
\code{\link{d_cancer_ger_mort_leuk_lymphW_i}}
}
\examples{
head(d_cancer_ger_incid_leuk_lymphW_i)
}
\keyword{datasets}
