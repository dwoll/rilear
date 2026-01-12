\name{interpol_mort_rate}
\alias{interpol_mort_rate}
\title{Interpolate mortality rates}
\description{
Interpolate mortality rates for broader age groups to rates for individual years of age. Uses spline interpolation for younger years and a simple Gompertz model for extrapolating to older years.
}
\usage{
interpol_mort_rate(x,
                   region = c("country", "regional"),
                   method = c("natural", "monoH.FC", "hyman"),
                   age = 0:100,
                   age_join = 50,
                   out = c("L", "W"))
}
\arguments{
  \item{x}{A data frame in long format giving sex-specific mortality rates. Should have variables \code{age_n} (the representative age for each age group), \code{sex} with levels \code{"f"} and \code{"m"}, \code{rate} for the mortality rate. If the rates represent a specific region, there should be a variable \code{ags} with an ID-code for each region.}
  \item{region}{Are rates for the whole country or for regional subdivisions?}
  \item{method}{Type of spline interpolation. See \code{\link[stats]{splinefun}}.}
  \item{age}{Years of age for which rates should be interpolated.}
  \item{age_join}{Age at which interpolation of younger ages (using splines) and older ages (using the Gompertz model) are joined.}
  \item{out}{Format of the output data frame. \code{"L"} for long format with respect to sex, \code{"W"} for wide format.}
}
\value{
A data frame either in long format (variables \code{age_n}, \code{sex}, \code{rate}) or wide format (variables \code{age_n}, \code{rate_f}, \code{rate_m}), possibly with the region identifier \code{ags}.
}
\details{
More elaborate methods for interpolating mortality rates exist, see R package Q2q (DOI: 10.32614/CRAN.package.Q2q). However for the purpose of using the rates in excess lifetime risk calculation simpler methods seem fine, provided that they consider the roughly exponential increase at old age.
}
\references{
.
}
\seealso{
\code{\link{interpol_cancer_rate}}
}
\examples{
d_mrL <- data.frame(age_n=c(0, seq(5, 85, by=5)),
                    sex  =factor(rep(c("f", "m"), each=18L))) |>
    transform(rate=exp(0.085*age_n - 10.3))

d_mrW_i <- interpol_mort_rate(d_mrL,
                              region="country",
                              age_join=40,
                              out="W")

head(d_mrW_i)
}
