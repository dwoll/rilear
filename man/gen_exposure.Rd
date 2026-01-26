\name{gen_exposure}
\alias{gen_exposure}
\title{Generate exposure events}
\description{
Convenience function to generate a list defining several radiation exposure events for use in lifetime excess absolute risk functions.
}
\usage{
gen_exposure(n, sex = c("f", "m"), agex, timing,
             dose_distr = "fixed",
             dose_param, dose_rate = "acute",
             ddref=1, cancer_site = "all_solid")
}
\arguments{
  \item{n}{Number of exposure events.}
  \item{sex}{Character, \code{"f"} or \code{"m"}.}
  \item{agex}{Age at exposure for the \code{n} exposure events. Useful for specifying the exposure of a single person in \code{\link{get_lear_indiv}} or \code{\link{get_lear_indiv_mc}}. Alternative to specifying \code{timing}.}
  \item{timing}{Assuming no fixed age at exposure, specify the timing of the exposure event in years. For example, \code{c(0, 2)} means that the second exposure event occurs 2 years after the first one. Alternative to specifying \code{agex}. Useful for specifying the exposure of a whole population in \code{\link{get_lear_pop}}.}
  \item{dose_distr}{Distribution to use for generating a dose for each exposure event. See \code{\link{sim_exposure}}.}
  \item{dose_param}{Parameter vector to use for generating \code{n} dose values in Gy or Sv, depending on \code{dose_distr}. See \code{\link{sim_exposure}}. To specify different parameter vectors for different exposure events, create a list of parameter vectors to pass to \code{dose_param}.}
  \item{dose_rate}{CAVE: Currently unused. Supported: \code{"acute"}, \code{"chronic"}.}
  \item{ddref}{Dose and dose rate effectiveness factor.}
  \item{cancer_site}{Relevant cancer for given exposure. See \code{\link{get_lear_indiv}}.}
}
\details{
Recycling rules are used if fewer than \code{n} entries for arguments are provided.
}
\value{
A list with one component for each exposure event. Each such component is a list with components \code{sex}, \code{agex} or \code{timing}, \code{dose_distr}, \code{dose_param}, \code{ddref}, and \code{cancer_site} that can be used as the first argument for \code{\link{sim_exposure}}.
}
\references{
.
}
\seealso{
\code{\link{sim_exposure}},
\code{\link{get_lear_indiv}},
\code{\link{get_lear_pop}}
}
\examples{
expo_event <- gen_exposure(n          =2,
                           sex        ="f",
                           agex       =c(20, 21),
                           dose_distr =c("fixed", "normal"),
                           dose_param =list(0.5, c(mean=0.3, sd=0.05)),
                           ddref      =c(1, 1.5),
                           dose_rate  =c("acute", "acute"),
                           cancer_site=list("all_solid",
                                            c("all_solid", "leuk_lymph")))

expo_event
}
