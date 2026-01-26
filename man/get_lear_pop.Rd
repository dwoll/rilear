\name{get_lear_pop}
\alias{get_lear_pop}
\title{Lifetime excess absolute risk for a population of exposed persons considering uncertainty}
\description{
Calculate the projected lifetime excess absolute cancer risk for a population of persons with radiation exposure. Supports multiple exposure events. Calculates LAR / LEAR / CER, REID / REIC, ELR, and RADS including the total number of projected cases. Uses Monte Carlo methods to take into account the uncertainty in several relevant parameters.
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
  \item{x}{A data frame defining the exposed population. Long format with respect to sex. Needs variables \code{age_n} with the numeric age, \code{sex} - a factor with levels \code{"f"} and \code{"m"}, and \code{pop} for the population size. For an example, see \code{\link{d_pop_ger_fedstate_2024L}}.}
  \item{n_sim}{Numer of simulated parameter settings for Monte Carlo approach to estimate uncertainty intervals. See \code{\link{gen_param_mc}}.}
  \item{exposure}{Parameters associated with the exposure event(s). See \code{\link{gen_exposure}} for convenience function to define. A list with one component for each exposure event. Each exposure event component is a list with the following components:
      \itemize{
        \item \code{sex}: Character, \code{"f"} or \code{"m"} - must be constant over exposure events.
        \item \code{timing}: Timing of exposure event as offset years relative to age at exposure given in rows of \code{x}.
        \item \code{dose}: In Gy or Sv.
        \item \code{ddref}: Dose- and Dose-Rate Effectiveness Factor with a default of 1.
        \item \code{dose_rate}: Character, \code{"acute"} or \code{"chronic"}.
        \item \code{cancer_site}: Character, one or multiple names of relevant cancer sites, corresponding to exposed organs. See \code{\link{get_lear_indiv}}.
      }
    }
  \item{wt_transfer}{A named list with one component per cancer site. Each component a named 2-vector giving the ERR / EAR weights used for risk transfer. This vector must have names \code{"ERR"} and \code{"EAR"}, and weights must be within [0, 1] as well as sum to 1.}
  \item{lat_t0}{A named list with one component per cancer site. Each component the central value For the S-shaped latency function.}
  \item{lat_eta}{A named list with one component per cancer site. Each component the width of the transition period from 0 to full risk for the S-shaped latency function. Used for argument \code{lat_method="ProZES"}.}
  \item{lat_fixed}{Set latency parameters \code{lat_t0} and \code{lat_eta} to a fixed value without uncertainty?}
  \item{wt_transfer_fixed}{Set \code{wt_transfer} weights to a fixed value without uncertainty?}
  \item{ddref_fixed}{Set \code{ddref} Dose- and Dose-Rate Effectiveness Factor to a fixed value without uncertainty?}
  \item{metric}{One or several risk metrics. See \code{\link{get_lear_indiv}}.}
  \item{stratify_sex}{Report number of projected cases separately for men and women?}
  \item{pop_ref}{Report number of projected cases per \code{pop_ref} persons.}
  \item{alpha}{Statistical significance level defining the width of the confidence interval as 1 - \code{alpha}.}
  \item{multicore}{Do multicore parallel processing?}
  \item{n_cores_max}{Maximum number of cores to use in multicore processing. See \code{\link[parallelly]{availableCores}}.}
  \item{n_cores_omit}{Number of cores not to use in multicore parallel processing. See \code{\link[parallelly]{availableCores}}.}
  \item{...}{All remaining arguments that get passed on to \code{\link{get_lear_indiv}}: \code{age_max}, \code{lat_method}, \code{base_cancer}, \code{base_cancer_mort} (if available), \code{d_base_mort}, \code{risk_model}, \code{risk_model_mort} (if available).}
}
\value{
A data frame with variables

\itemize{
\item \code{site}: Cancer site.
\item \code{metric}: The lifetime excess absolute risk metric.
\item \code{sex}: For \code{stratify_sex=TRUE}.
\item \code{pop}: Population size for which projected absolute number of cases is estimated.
\item \code{pop_ref}: Reference population size for which projected absolute number of cases is estimated.
\item \code{mean_rsk}: Mean risk.
\item \code{median_rsk}: Median risk.
\item \code{CIlo_rsk}: Confidence interval lower boundary risk.
\item \code{CIup_rsk}: Confidence interval upper boundary risk.
\item \code{mean_abs}: Mean number of projected absolute number of cases in \code{pop}.
\item \code{median_abs}: Median number of projected absolute number of cases in \code{pop}.
\item \code{PIlo_abs}: Prediction interval lower boundary projected absolute number of cases in \code{pop}.
\item \code{PIup_abs}: Prediction interval upper boundary projected absolute number of cases in \code{pop}.
\item \code{mean_ref}: Mean number of projected absolute number of cases per \code{pop_ref}.
\item \code{median_ref}: Median number of projected absolute number of cases per \code{pop_ref}.
\item \code{PIlo_ref}: Prediction interval lower boundary projected absolute number of cases per \code{pop_ref}.
\item \code{PIup_ref}: Prediction interval upper boundary projected absolute number of cases per \code{pop_ref}.
}
}
\details{
CAVE: All calculations preliminary, not yet validated!

See \code{\link{gen_param_mc}} for the implementation of Monte Carlo methods for estimating the uncertainty of the lifetime risk estimate.
}
\references{
.
}
\seealso{
\code{\link{gen_exposure}},
\code{\link{gen_param_mc}},
\code{\link{get_lear_indiv}},
\code{\link{get_lear_indiv_mc}},
\code{\link{d_pop_ger_fedstate_2024L}}
}
\examples{
expo_event <- gen_exposure(n          =2,
                           sex        ="f",
                           timing     =1,
                           dose_distr =c("normal", "fixed"),
                           dose_param =list(c(mean=0.5, sd=0.015), 0.5),
                           ddref      =c(1, 1),
                           # dose_rate  ="acute",
                           cancer_site=list("all_solid",
                                            c("all_solid", "leuk_lymph")))

## generate sets of parameter settings
## with cancer mortality
rm <- list(all_solid =rm_solid_incid_walsh2021(),
           breast    =rm_breast_incid_walsh2021(),
           leuk_lymph=rm_leuk_incid_walsh2021())

base_cancer <- list(all_solid =d_cancer_ger_incid_solidW_i,
                    breast    =d_cancer_ger_incid_breastW_i,
                    leuk_lymph=d_cancer_ger_incid_leuk_lymphW_i)

wt_transfer <- list(all_solid =c(ERR=0.5, EAR=0.5),
                    breast    =c(ERR=0.0, EAR=1.0),
                    leuk_lymph=c(ERR=1.0, EAR=0.0))

lat_t0 <- list(all_solid =5,
               breast    =5,
               leuk_lymph=1.5)

lat_eta <- list(all_solid =6,
                breast    =6,
                leuk_lymph=6.75)

## solid cancer risk estimates
## for exposure of the population of Germany
get_lear_pop(x                =d_pop_ger_country_2024L,
             n_sim            =10L,
             exposure         =expo_event,
             wt_transfer      =wt_transfer,
             lat_t0           =lat_t0,
             lat_eta          =lat_eta,
             lat_method       ="ProZES",
             lat_fixed        =FALSE,
             wt_transfer_fixed=FALSE,
             ddref_fixed      =FALSE,
             risk_model       =rm,
             stratify_sex     =TRUE,
             pop_ref          =100000,
             alpha            =0.05,
             multicore        =FALSE,
             n_cores_max      =10L,
             n_cores_omit     =2L,
             base_cancer      =base_cancer,
             d_base_mort      =d_lifetable_ger_2024W,
             metric           =c("LEAR", "RADS"),
             age_max          =90)
}
