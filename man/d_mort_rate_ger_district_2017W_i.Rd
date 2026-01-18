\name{d_mort_rate_ger_district_2017W_i}
\alias{d_mort_rate_ger_district_2017W_i}
\docType{data}
\title{Overall mortality rates per district in Germany 2017}
\description{
Overall mortality rates in districts in Germany in 2017, interpolated to yearly attained age, per 1 PY. Data frame in wide format with respect to sex.}
\usage{data(d_mort_rate_ger_district_2017W_i)}
\format{
  Data frame with 4 variables.
  \describe{
    \item{\code{ags}}{ID for district.}
    \item{\code{age_n}}{Age from 0 to 100 in steps of 1 year.}
    \item{\code{rate_f}}{Mortality rate per 1 PY for women.}
    \item{\code{rate_m}}{Mortality rate per 1 PY for men.}
  }
}
\details{
Data source: Destatis table 12411-04-02-4 (population) and 12613-02-02-4 (mortality) \url{https://www.regionalstatistik.de/}
}
\seealso{
\code{\link{interpol_mort_rate}},
\code{\link{get_lear1}},
\code{\link{get_lear_pop}},
\code{\link{d_lifetable_ger_2024W}},
\code{\link{d_mort_rate_ger_country_2024W_i}},
\code{\link{d_mort_rate_ger_fedstate_2017W_i}}
}
\examples{
head(d_mort_rate_ger_district_2017W_i)
}
\keyword{datasets}
