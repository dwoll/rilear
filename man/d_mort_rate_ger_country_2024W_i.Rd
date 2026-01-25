\name{d_mort_rate_ger_country_2024W_i}
\alias{d_mort_rate_ger_country_2024W_i}
\docType{data}
\title{Overall mortality rates Germany 2024}
\description{
Overall mortality rates in Germany in 2024, interpolated to yearly attained age, per 1 PY. Data frame in wide format with respect to sex.}
\usage{data(d_mort_rate_ger_country_2024W_i)}
\format{
  Data frame with 3 variables.
  \describe{
    \item{\code{age_n}}{Age from 0 to 100 in steps of 1 year.}
    \item{\code{rate_f}}{Mortality rate per 1 PY for women.}
    \item{\code{rate_m}}{Mortality rate per 1 PY for men.}
  }
}
\details{
Data source: Destatis tables 12411-0013 (population) and 12613-0003 (mortality) \url{https://www-genesis.destatis.de/}
}
\seealso{
\code{\link{get_lear_indiv}},
\code{\link{get_lear_pop}},
\code{\link{d_lifetable_ger_2024W}},
\code{\link{d_mort_rate_ger_district_2017W_i}},
\code{\link{d_mort_rate_ger_fedstate_2017W_i}}
}
\examples{
head(d_mort_rate_ger_country_2024W_i)
}
\keyword{datasets}
