\name{d_pop_ger_district_2024L}
\alias{d_pop_ger_district_2024L}
\docType{data}
\title{Population per district in Germany 2024}
\description{
Population in districts in Germany in 2017. Data frame in long format with respect to sex.}
\usage{data(d_pop_ger_district_2024L)}
\format{
  Data frame with 6 variables.
  \describe{
    \item{\code{ags}}{ID for district.}
    \item{\code{district}}{Name of district.}
    \item{\code{age_f}}{Age as a factor with levels from 0 to 74 in steps of 1 year, then 75-79, 80-84, 85-89, 90+.}
    \item{\code{age_n}}{Age as numeric representation of the age groups.}
    \item{\code{sex}}{Sex, factor with levels \code{"f"} (female) and \code{"m"} (male).}
    \item{\code{pop}}{Population count.}
  }
}
\details{
Data source: Destatis table 12411-04-02-4 \url{https://www.regionalstatistik.de/}
}
\seealso{
\code{\link{get_lear_pop}},
\code{\link{d_pop_ger_country_2024L}},
\code{\link{d_pop_ger_fedstate_2024L}}
}
\examples{
head(d_pop_ger_district_2024L)
}
\keyword{datasets}
