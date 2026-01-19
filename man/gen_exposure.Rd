\name{gen_exposure}
\alias{gen_exposure}
\title{Generate exposure events}
\description{
Convenience function to generate a list of radiation exposure events for use in lifetime excess absolute risk functions.
}
\usage{
gen_exposure(n, agex, timing,
             dose_distr = c("fixed", "normal", "lognormal",
                            "triangular", "logtriangular", 
                            "uniform", "loguniform"),
             dose_param,
             dose_rate=c("acute", "chronic"),
             ddref=1)
}
\arguments{
  \item{n}{Number of exposure events.}
  \item{agex}{Age at exposure for the \code{n} exposure events. Useful for specifying the exposure of a single person in \code{\link{get_lear1}}. Alternative to specifying \code{timing}. Recycling rules are used if fewer than \code{n} entries for \code{agex} are provided.}
  \item{timing}{Assuming no fixed age at exposure, specify the timing of the exposure event in years. For example, \code{c(1, 3)} means that the second exposure event occurs \code{3-1 = 2} years after the first one. Alternative to specifying \code{agex}. Useful for specifying the exposure of a whole population in \code{\link{get_lear_pop}}. Recycling rules are used if fewer than \code{n} entries for \code{timing} are provided.}
  \item{dose_distr}{Distribution to use for generating a dose for each exposure event. \code{"fixed"} for a fixed dose value, random sampling of \code{n} values is done for the other options. Recycling rules are used if fewer than \code{n} entries for \code{dose_distr} are provided.}
  \item{dose_param}{Parameter vector to use for generating \code{n} dose values in Gy or Sv. For \code{dose_distr} = \code{"fixed"} the value to use. For \code{dose_distr} = \code{"normal"} a vector giving the mean and sd. For \code{"lognormal"} the geometric mean and geometric sd. For \code{"triangular"} the mode, minimum, and maximum; for \code{"logtriangular"} the log mode, log minimum and log maximum. For \code{"uniform"} the minimum and maximum. For \code{"loguniform"} the log minimum and the log maximum. To specify different parameter vectors for different exposure events, create a list of parameter vectors to pass to \code{dose_param}. Recycling rules are used if fewer than \code{n} elements or components for \code{dose_param} are provided.}
  \item{dose_rate}{Distribution to use for generating a dose for each exposure event. \code{"fixed"} for a fixed dose value, random sampling of \code{n} values is done for the other options. Recycling rules are used if fewer than \code{n} entries for \code{dose_distr} are provided.}
  \item{ddref}{Dose and dose rate effectiveness factor. Recycling rules are used if fewer than \code{n} entries for \code{ddref} are provided.}
}
\value{
A list with one component for each exposure event. Each such component is a list with components \code{agex} or \code{timing}, \code{dose_distr}, and \code{dose_param}.
}
\references{
.
}
\seealso{
\code{\link{get_lear1}},
\code{\link{get_lear_pop}}
}
\examples{
event <- gen_exposure(n     =3,
                      timing=1,
                      dose_distr=c("normal", "triangular"),
                      dose_param=list(c(mean=0.5, sd=0.05),
                                      c(mode=0.5, min=0.1, max=0.6)),
                      dose_rate ="acute",
                      ddref     =1)

event
}
