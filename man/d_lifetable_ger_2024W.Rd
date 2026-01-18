\name{d_lifetable_ger_2024W}
\alias{d_lifetable_ger_2024W}
\docType{data}
\title{Lifetable Germany 2022-2024}
\description{
Lifetable Germany based on data from 2022-2024.}
\usage{data(d_lifetable_ger_2024W)}
\format{
  Data frame with 15 variables.
  \describe{
    \item{\code{age_n}}{Age from 0 to 100 in steps of 1 year.}
    \item{\code{q_f}}{Probability of death, female.}
    \item{\code{p_f}}{Probability of survival, female.}
    \item{\code{I_f}}{Number of survivors, female.}
    \item{\code{d_f}}{Deaths, female.}
    \item{\code{L_f}}{Person years until age x+1, female.}
    \item{\code{T_f}}{Total remaining person years, female.}
    \item{\code{e_f}}{Average remaining life expectancy, female.}
    \item{\code{q_m}}{Probability of death, male.}
    \item{\code{p_m}}{Probability of survival, male.}
    \item{\code{I_m}}{Number of survivors, male.}
    \item{\code{d_m}}{Deaths, male.}
    \item{\code{L_m}}{Person years until age age_e+1, male.}
    \item{\code{T_m}}{Total remaining person years, male.}
    \item{\code{e_m}}{Average remaining life expectancy, male.}
  }
}
\details{
Data source: Destatis table 12621-0001 \url{https://www-genesis.destatis.de/}
}
\seealso{
\code{\link{get_lear1}},
\code{\link{get_lear_pop}},
\code{\link{d_mort_rate_ger_country_2024W_i}},
\code{\link{d_mort_rate_ger_district_2017W_i}},
\code{\link{d_mort_rate_ger_fedstate_2017W_i}}
}
\examples{
head(d_lifetable_ger_2024W)
}
\keyword{datasets}
