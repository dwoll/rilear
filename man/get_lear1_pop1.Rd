\name{get_lear1_pop1}
\alias{get_lear1_pop1}
\title{Lifetime excess absolute risk for a population of exposed persons without considering uncertainty}
\description{
Calculate the lifetime excess absolute cancer risk for a single person with radiation exposure. Supports multiple exposure events. Calculates LAR / LEAR / CER, REID / REIC, ELR, and RADS.
}
\usage{
get_lear1_pop1(x, l_param, age_max, metric, ...)
}
\arguments{
  \item{x}{One observation = one row of a data frame defining the exposed population. Needs variables \code{age_n} with the numeric age, \code{sex} - a factor with levels \code{"f"} and \code{"m"}, and \code{pop} for the population size. For an example, see \code{\link{d_pop_ger_fedstate_2024L}}.}
  \item{l_param}{A list of parameters that are associated with uncertainty, including components \code{exposure}, \code{ddref}, \code{param_err}, \code{param_ear}, \code{param_err_mort}, \code{param_ear_mort}, \code{wt_transfer}, \code{lat_t0}, \code{lat_eta}. Component \code{exposure} needs to be a list with two vectors of the same length: \code{agex_timing} for the timing of each exposure event and \code{dose} for the corresponding dose at each exposure event, given in Gy or Sv. Components \code{param_err} and \code{param_ear} each are a parameter vector for the ERR / EAR risk models defined in \code{risk_model}. Likewise, components \code{param_err_mort} and \code{param_ear_mort} each are a parameter vector for the ERR / EAR risk models defined in \code{risk_model_mort}. Component \code{wt_transfer} is a 2-vector giving the ERR / EAR weights used for risk transfer. This vector must have names \code{"ERR"} and \code{"EAR"}, and weights must be within [0, 1] as well as sum to 1. Component \code{ddref} defines the dose- and dose-rate effectiveness factor (DDREF) with a default of 1. Components \code{lat_t0} and \code{lat_eta} are used for the S-shaped latency function with \code{t0} being the central value and \code{lat_eta} defining the width of the transition period from 0 to full risk if argument \code{lat_method} is set to \code{"ProZES"}.}
  \item{age_max}{Sum excess risk up to including this attained age.}
  \item{metric}{One or several risk metrics. See \code{\link{get_lear1}}.}
  \item{...}{All remaining arguments that get passed on to \code{\link{get_lear1}}: \code{lat_method}, \code{d_base_cancer}, \code{d_base_cancer_mort} (if available), \code{d_base_mort}, \code{risk_model}, \code{risk_model_mort} (if available).}
}
\value{
A data frame with variables

\itemize{
\item \code{metric}: The lifetime excess absolute risk metric
\item \code{sex}: Sex, \code{"f"} or \code{"m"}
\item \code{age_f}: Age as a factor (from \code{d_pop})
\item \code{age_n}: Numeric age (from \code{d_pop})
\item \code{pop}: Population size
\item \code{value}: Value of the lifetime excess absolute risk metric
}
}
\details{
CAVE: All calculations preliminary, not yet validated!
}
\references{
.
}
\seealso{
\code{\link{gen_exposure}},
\code{\link{get_lear1}},
\code{\link{get_lear_pop}},
\code{\link{get_lear1_pop}},
\code{\link{d_pop_ger_fedstate_2024L}}
}
\examples{
## ERR / EAR risk models for solid cancer incidence and mortality
rm_incid <- rm_solid_incid_walsh2021()
rm_mort  <- rm_solid_mort_sumray()

## solid cancer risk estimates
## for exposure of the population of Germany
get_lear1_pop1(x=d_pop_ger_country_2024L[47, ],
               l_param=list(exposure  =list(dose=c(0.5, 0.7),
                                            agex_timing=c(0, 1)),
                        param_err     =rm_incid$err$param,
                        param_ear     =rm_incid$ear$param,
                        param_err_mort=rm_mort$err$param,
                        param_ear_mort=rm_mort$ear$param,
                        wt_transfer   =c(ERR=0.5, EAR=0.5),
                        ddref         =1,
                        lat_t0        =5,
                        lat_eta       =6.25),
               age_max           =90,
               metric            =c("LEAR", "RADS"),
               lat_method        ="ProZES",
               risk_model        =rm_solid_incid_walsh2021(),
               risk_model_mort   =rm_solid_mort_sumray(),
               d_base_cancer     =d_cancer_ger_incid_solidW_i,
               d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
               d_base_mort       =d_lifetable_ger_2024W)
}
