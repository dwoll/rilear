\name{interpol_cancer_rate}
\alias{interpol_cancer_rate}
\title{Interpolate cancer rates}
\description{
Interpolate cancer rates for broader age groups to rates for individual years of age. Uses interpolation by natural splines.}
\usage{
interpol_cancer_rate(x,
                     region = c("country", "regional"),
                     age=0:100,
                     out=c("L", "W"))
}
\arguments{
  \item{x}{A data frame in long format giving sex-specific cancer rates. Should have variables \code{age_n} (the representative age for each age group), \code{sex} with levels \code{"f"} and \code{"m"}, \code{rate} for the cancer rate. If the rates represent a specific region, there should be a variable \code{ags} with an ID-code for each region.}
  \item{region}{Are rates for the whole country or for regional subdivisions?}
  \item{age}{Years of age for which rates should be interpolated.}
  \item{out}{Format of the output data frame. \code{"L"} for long format with respect to sex, \code{"W"} for wide format.}
}
\value{
A data frame either in long format (variables \code{age_n}, \code{sex}, \code{rate}) or wide format (variables \code{age_n}, \code{rate_f}, \code{rate_m}), possibly with the region identifier \code{ags}.
}
\references{
.
}
\seealso{
\code{\link{interpol_mort_rate}}
}
\examples{
d_crL <- data.frame(age_n=c(0, seq(5, 85, by=5)),
                    sex  =factor(rep(c("f", "m"), each=18L))) |>
    transform(rate=exp(0.065*age_n - 9.45))

d_crW_i <- interpol_cancer_rate(d_crL,
                                region="country",
                                out="W")

head(d_crW_i)

}
