\name{get_lear_pop}
\alias{get_lear_pop}
\title{Lifetime excess absolute risk for a population of exposed persons considering uncertainty}
\description{
Calculate the lifetime excess absolute cancer risk for a population of persons with radiation exposure. Supports multiple exposure events. Calculates LAR / LEAR / CER, REID / REIC, ELR, and RADS including the total number of expected cases. Uses Monte Carlo methods to take into account the uncertainty in several relevant parameters.
}
\usage{
get_lear_pop(x, n_sim = 1000L, exposure, wt_transfer,
             lat_t0, lat_eta, lat_fixed = FALSE,
             wt_transfer_fixed = FALSE, ddref_fixed = FALSE,
             metric = c("LAR", "LEAR", "CER",
                        "REID", "REIC",
                        "ELR", "RADS"),
             stratify_sex = FALSE, pop_ref = 100000,
             alpha = 0.05, multicore = FALSE, n_cores_max = 10L,
             n_cores_omit = 2L, ...)
}
\arguments{
  \item{x}{A data frame defining the exposed population. Needs variables \code{age_n} with the numeric age, \code{sex} - a factor with levels \code{"f"} and \code{"m"}, and \code{pop} for the population size. For an example, see \code{\link{d_pop_ger_fedstate_2024L}}.}
  \item{n_sim}{Numer of simulated parameter settings for Monte Carlo approach to estimate uncertainty interval. See \code{\link{get_lear_indiv_mc}}.}
  \item{exposure}{A list defining the exposure events with one component for each such event. Each component defining a single exposure event is a list with components \code{timing} for the years of exposure following the age given in \code{x}, \code{distr} for the dose distribution class, and \code{param} with the parameters necessary to specify the dose distribution. See \code{\link{gen_exposure}} - a utility function that may be used to create such a list.}
  \item{wt_transfer}{A 2-vector giving the ERR / EAR weights used for risk transfer. This vector must have names \code{"ERR"} and \code{"EAR"}, and weights should be within [0, 1] as well as sum to 1. Otherwise, values will be re-scaled.}
  \item{lat_t0}{The central value For the S-shaped latency function.}
  \item{lat_eta}{The width of the transition period from 0 to full risk for the S-shaped latency function. Used for argument \code{lat_method="ProZES"}.}
  \item{lat_fixed}{Set latency parameters \code{lat_t0} and \code{lat_eta} to a fixed value without uncertainty?}
  \item{wt_transfer_fixed}{Set \code{wt_transfer} weights to a fixed value without uncertainty?}
  \item{ddref_fixed}{Set \code{ddref} Dose- and Dose-Rate Effectiveness Factor to a fixed value without uncertainty?}
  \item{metric}{One or several risk metrics. See \code{\link{get_lear_indiv}}.}
  \item{stratify_sex}{Report number of expected cases separately for men and women?}
  \item{pop_ref}{Report number of expected cases per \code{pop_ref} persons.}
  \item{alpha}{Statistical significance level defining the width of the confidence interval as 1 - \code{alpha}.}
  \item{multicore}{Do multicore parallel processing?}
  \item{n_cores_max}{Maximum number of cores to use in multicore processing. See \code{\link[parallelly]{availableCores}}.}
  \item{n_cores_omit}{Number of cores not to use in multicore parallel processing. See \code{\link[parallelly]{availableCores}}.}
  \item{...}{All remaining arguments that get passed on to \code{\link{get_lear_indiv}}: \code{age_max}, \code{lat_method}, \code{d_base_cancer}, \code{d_base_cancer_mort} (if available), \code{d_base_mort}, \code{risk_model}, \code{risk_model_mort} (if available).}
}
\value{
A data frame with variables

\itemize{
\item \code{metric}: The lifetime excess absolute risk metric
\item \code{sex}: For \code{stratify_sex=TRUE}
\item \code{pop}: Population size for which expected absolute number of cases is estimated
\item \code{pop_ref}: Reference population size for which expected absolute number of cases is estimated
\item \code{mean_rsk}: Mean risk
\item \code{median_rsk}: Median risk
\item \code{CIlo_rsk}: Confidence interval lower boundary risk
\item \code{CIup_rsk}: Confidence interval upper boundary risk
\item \code{mean_abs}: Mean number of expected absolute number of cases in \code{pop}
\item \code{median_abs}: Median number of expected absolute number of cases in \code{pop}
\item \code{PIlo_abs}: Prediction interval lower boundary expected absolute number of cases in \code{pop}
\item \code{PIup_abs}: Prediction interval upper boundary expected absolute number of cases in \code{pop}
\item \code{mean_ref}: Mean number of expected absolute number of cases per \code{pop_ref}
\item \code{median_ref}: Median number of expected absolute number of cases per \code{pop_ref}
\item \code{PIlo_ref}: Prediction interval lower boundary expected absolute number of cases per \code{pop_ref}
\item \code{PIup_ref}: Prediction interval upper boundary expected absolute number of cases per \code{pop_ref}
}
}
\details{
CAVE: All calculations preliminary, not yet validated!

See \code{\link{get_lear_indiv_mc}} for the implementation of Monte Carlo methods for estimating the uncertainty of the lifetime risk estimate.
}
\references{
.
}
\seealso{
\code{\link{gen_exposure}},
\code{\link{get_lear_indiv}},
\code{\link{get_lear_indiv_mc}},
\code{\link{d_pop_ger_fedstate_2024L}}
}
\examples{
## solid cancer risk estimates
## for exposure of the population of Germany
get_lear_pop(x       =d_pop_ger_country_2024L,
             n_sim   =100L,
             exposure=gen_exposure(n     =1,
                                   timing=1,
                                   ddref =1,
                                   dose_rate ="acute",
                                   dose_distr="normal",
                                   dose_param=list(c(mean=0.5,
                                                     sd=0.1))),
             wt_transfer       =c(ERR=0.5, EAR=0.5),
             lat_t0            =5,
             lat_eta           =6,
             lat_method        ="ProZES",
             lat_fixed         =FALSE,
             wt_transfer_fixed =FALSE,
             ddref_fixed       =FALSE,
             risk_model        =rm_solid_incid_walsh2021(),
             risk_model_mort   =rm_solid_mort_sumray(),
             stratify_sex      =TRUE,
             pop_ref           =100000,
             alpha             =0.05,
             multicore         =FALSE,
             n_cores_max       =10L,
             n_cores_omit      =2L,
             d_base_cancer     =d_cancer_ger_incid_solidW_i,
             d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
             d_base_mort       =d_lifetable_ger_2024W,
             metric            =c("LEAR", "RADS"),
             age_max           =90)
}
