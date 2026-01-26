\name{sim_exposure}
\alias{sim_exposure}
\title{Simulate dose values for exposure event}
\description{
Function to simulate dose values for a given exposure event.
}
\usage{
sim_exposure(x, n_sim = 1L, ddref_fixed = FALSE)
}
\arguments{
  \item{x}{List with parameters defining the exposure event:
    \itemize{
      \item \code{sex}: Character, \code{"f"} or \code{"m"}.
      \item \code{agex}: Age at exposure of the exposed person. Alternative to \code{timing}.
      \item \code{timing}: Timing of exposure event in years. Alternative to \code{agex}. Used for \code{\link{get_lear_pop}}.
      \item \code{dose_distr}: Distribution to use for generating a dose. Supported: \code{"fixed"}, \code{"normal"}, \code{"lognormal"}, \code{"triangular"}, \code{"logtriangular"}, \code{"uniform"}, \code{"loguniform"}. \code{"fixed"} for a fixed dose value, random sampling is done for the other options.
      \item \code{dose_param}: Parameter vector to use for generating \code{n_sim} dose values in Gy or Sv depending on \code{dose_distr}:
        \itemize{
          \item \code{"fixed"}: The dose value to use.
          \item \code{"normal"}: A vector giving the mean and sd of the dose.
          \item \code{"lognormal"}: A vector giving the geometric mean and geometric sd of the dose.
          \item \code{"triangular"}: A vector giving the mode, minimum, and maximum dose.
          \item \code{"logtriangular"}: A vector giving the log mode, log minimum and log maximum dose.
          \item \code{"uniform"}: A vector giving the minimum and maximum dose.
          \item \code{"loguniform"}: A vector giving the log minimum and log maximum dose.
        }
        \item \code{dose_rate}: CAVE: Currently unused. Supported: \code{"acute"}, \code{"chronic"}.
        \item \code{ddref}: Dose- and Dose Rate Effectiveness Factor.
        \item \code{cancer_site}: Vector of relevant cancer sites for given exposure. See \code{\link{get_lear_indiv}}.
      }
  }
  \item{n_sim}{Number of simulated dose values.}
  \item{ddref_fixed}{Set \code{ddref} Dose- and Dose-Rate Effectiveness Factor to a fixed value?}
}
\details{
.
}
\value{
A list with these components:
\itemize{
    \item \code{sex}: Vector with \code{n_sim} elements specifying the sex of the exposed person.
    \item \code{agex_timing}: Vector with \code{n_sim} elements specifying the age of exposure of the exposed person.
    \item \code{dose}: Vector with \code{n_sim} elements specifying the radiation dose.
    \item \code{ddref}: Vector with \code{n_sim} elements specifying the Dose- and Dose Rate Effectiveness Factor.
    \item \code{dose_rate}: Vector with \code{n_sim} elements specifying the dose rate.
    \item \code{cancer_site}: List with \code{n_sim} components specifying the relevant cancer sites corresponding to the exposed organs.
  }
}
\references{
.
}
\seealso{
\code{\link{gen_param_mc}},
\code{\link{gen_exposure}},
\code{\link{get_lear_indiv_mc}},
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

sim_exposure(expo_event[[1]], n_sim=5L, ddref_fixed=FALSE)
}
