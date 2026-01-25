\name{get_lear_indiv_mc}
\alias{get_lear_indiv_mc}
\title{Lifetime excess absolute risk for a single exposed person considering uncertainty}
\description{
Calculate the lifetime excess absolute cancer risk for a single person with radiation exposure. Supports multiple exposure events. Calculates LAR / LEAR / CER, REID / REIC, ELR, and RADS including the total number of expected cases. Uses Monte Carlo methods to take into account the uncertainty in several relevant parameters.
}
\usage{
get_lear_indiv_mc(exposure, sex = c("f", "m"), n_sim = 1000L,
                  wt_transfer, lat_t0, lat_eta, lat_fixed = FALSE,
                  wt_transfer_fixed = FALSE, ddref_fixed = FALSE,
                  metric = c("LAR", "LEAR", "CER",
                             "REID", "REIC",
                             "ELR", "RADS"),
                  alpha = 0.05, multicore = FALSE, n_cores_max = 10L,
                  n_cores_omit = 2L, aggr_mc = TRUE, ...)
}
\arguments{
  \item{exposure}{A list with two vectors of the same length: \code{agex} for the age at each exposure event and \code{dose} for the corresponding dose at each exposure event, given in Gy or Sv.}
  \item{sex}{Value from a factor, either \code{"f"} or \code{"m"}.}
  \item{n_sim}{Numer of simulated parameter settings for Monte Carlo approach to estimate uncertainty interval. See \code{\link{get_lear_indiv_mc}}.}
  \item{wt_transfer}{A 2-vector giving the ERR / EAR weights used for risk transfer. This vector must have names \code{"ERR"} and \code{"EAR"}, and weights should be within [0, 1] as well as sum to 1. Otherwise, values will be re-scaled.}
  \item{lat_t0}{The central value For the S-shaped latency function.}
  \item{lat_eta}{The width of the transition period from 0 to full risk for the S-shaped latency function. Used for argument \code{lat_method="ProZES"}.}
  \item{lat_fixed}{Set latency parameters \code{lat_t0} and \code{lat_eta} to a fixed value without uncertainty?}
  \item{wt_transfer_fixed}{Set \code{wt_transfer} weights to a fixed value without uncertainty?}
  \item{ddref_fixed}{Set \code{ddref} Dose- and Dose-Rate Effectiveness Factor to a fixed value without uncertainty?}
  \item{metric}{One or several risk metrics. See \code{\link{get_lear_indiv}}.}
  \item{alpha}{Statistical significance level defining the width of the confidence interval as 1 - \code{alpha}.}
  \item{multicore}{Do multicore parallel processing?}
  \item{n_cores_max}{Maximum number of cores to use in multicore processing. See \code{\link[parallelly]{availableCores}}.}
  \item{n_cores_omit}{Number of cores not to use in multicore parallel processing. See \code{\link[parallelly]{availableCores}}.}
  \item{aggr_mc}{Aggregate MC distribution using mean, median, CI? If not, all MC results are returned.}
  \item{...}{All remaining arguments that get passed on to \code{\link{get_lear_indiv}}: \code{age_max}, \code{lat_method}, \code{d_base_cancer}, \code{d_base_cancer_mort} (if available), \code{d_base_mort}, \code{risk_model}, \code{risk_model_mort} (if available).}
}
\value{
A data frame with variables

\itemize{
\item \code{metric}: The lifetime excess absolute risk metric
\item \code{mean_rsk}: Mean risk
\item \code{median_rsk}: Median risk
\item \code{CIlo_rsk}: Confidence interval lower boundary risk
\item \code{CIup_rsk}: Confidence interval upper boundary risk
}
}
\details{
CAVE: All calculations preliminary, not yet validated!

All risk metrics are defined in Sommer et al. 2025:

\itemize{
\item \code{"LAR"} = \code{"LEAR"} = \code{"CER"}: Lifetime Excess Absolute Risk / Cumulative Excess Risk. Calculated assuming that overall survival of the exposed is practically the same as baseline survival. Hence, \code{risk_model_mort}, \code{param_err_mort}, \code{param_ear_mort}, and \code{d_base_cancer_mort} need not be specified. Plausible for low-dose exposures.
\item \code{"REID"} = \code{"REIC"}: Risk of Exposure-Induced Death / Cancer (depending on the provided cancer risk model and baseline cancer rates). Calculated assuming that overall survival of the exposed can be different than baseline survival. Hence, \code{risk_model_mort}, \code{param_err_mort}, \code{param_ear_mort}, and \code{d_base_cancer_mort} must be specified.
\item \code{"ELR"}: Excess Lifetime Risk. Calculated assuming that overall survival of the exposed can be different than baseline survival. Hence, \code{risk_model_mort}, \code{param_err_mort}, \code{param_ear_mort}, and \code{d_base_cancer_mort} must be specified.
\item \code{"RADS"}: Radiation Associated Decrease in Survival. Calculated assuming that overall survival of the exposed can be different than baseline survival. Hence, \code{risk_model_mort}, \code{param_err_mort}, \code{param_ear_mort}, and \code{d_base_cancer_mort} must be specified.
}
}
\references{
.
}
\seealso{
\code{\link{gen_exposure}},
\code{\link{get_lear_indiv}},
\code{\link{get_lear_pop}}
}
\examples{
## use convenience function to generate list of exposure events
expo_event <- gen_exposure(n         =1,
                           agex      =20,
                           dose_distr=c("normal"),
                           dose_param=c(mean=0.1, sd=0.015),
                           dose_rate ="acute",
                           ddref     =1)

## solid cancer risk estimates
get_lear_indiv_mc(exposure          =expo_event,
                  sex               ="f",
                  n_sim             =100L,
                  wt_transfer       =c(ERR=0.5, EAR=0.5),
                  lat_t0            =5,
                  lat_eta           =6,
                  lat_method        ="ProZES",
                  lat_fixed         =FALSE,
                  wt_transfer_fixed =FALSE,
                  ddref_fixed       =FALSE,
                  ##
                  risk_model        =rm_solid_incid_walsh2021(),
                  risk_model_mort   =rm_solid_mort_sumray(),
                  ##
                  alpha             =0.05,
                  multicore         =FALSE,
                  n_cores_max       =10L,
                  n_cores_omit      =2L,
                  aggr_mc           =TRUE,
                  ##
                  d_base_cancer     =d_cancer_ger_incid_solidW_i,
                  d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
                  d_base_mort       =d_lifetable_ger_2024W,
                  metric            =c("LEAR", "RADS"),
                  ##
                  age_max           =90)
}
