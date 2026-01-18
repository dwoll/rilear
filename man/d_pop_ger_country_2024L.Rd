\name{d_pop_ger_country_2024L}
\alias{d_pop_ger_country_2024L}
\docType{data}
\title{Population Germany 2024}
\description{
Population Germany in 2024. Data frame in long format with respect to sex.}
\usage{data(d_pop_ger_country_2024L)}
\format{
  Data frame with 3 variables.
  \describe{
    \item{\code{age_f}}{Age as a factor with levels from 0 to 89 in steps of 1 year, and 90+.}
    \item{\code{age_n}}{Age as numeric representation of the age groups.}
    \item{\code{sex}}{Sex, factor with levels \code{"f"} (female) and \code{"m"} (male).}
    \item{\code{pop}}{Population count.}
  }
}
\details{
Data source: Destatis table 12411-0013 \url{https://www-genesis.destatis.de/}
}
\seealso{
\code{\link{get_lear_pop}},
\code{\link{d_pop_ger_district_2024L}},
\code{\link{d_pop_ger_fedstate_2024L}}
}
\examples{
head(d_pop_ger_country_2024L)
}
\keyword{datasets}
