\name{get_elr_pop}
\alias{get_elr_pop}
\title{Lifetime excess absolute risk for a population of exposed persons considering uncertainty}
\description{
Calculate the lifetime excess absolute cancer risk for a population of persons with radiation exposure. Supports multiple exposure events. Calculates LAR / LEAR / CER, REID / REIC, ELR, and RADS including the total number of expected cases. Uses Monte Carlo methods to take into account the uncertainty in several relevant parameters.
}
\usage{
get_elr_pop(x, exposure, stratify_sex = FALSE, pop_ref = 100000,
            alpha = 0.05, multicore = FALSE, n_cores_max = 10L,
            n_cores_omit = 2L, ...)
}
\arguments{
  \item{x}{A data frame defining the exposed population. Needs variables \code{age_n} with the numeric age, \code{sex} - a factor with levels \code{"f"} and \code{"m"}, and \code{pop} for the population size. For an example, see \code{\link{d_pop_ger_fedstate_2024L}}.}
  \item{exposure}{A list defining the exposure events with one component for each such event. Each component defining a single exposure event is a list with components \code{timing} for the years of exposure following the age given in \code{x}, \code{distr} for the dose distribution class, and \code{param} with the parameters necessary to specify the dose distribution. See \code{\link{gen_exposure}} - a utility function that may be used to create such a list.}
  \item{stratify_sex}{Report number of expected cases separately for men and women?}
  \item{pop_ref}{Report number of expected cases per \code{pop_ref} persons.}
  \item{alpha}{Statistical significance level defining the width of the confidence interval as 1 - alpha.}
  \item{multicore}{Do multicore parallel processing?}
  \item{n_cores_max}{Maximum number of cores to use in multicore processing. See \code{\link[parallelly]{availableCores}}.}
  \item{n_cores_omit}{Number of cores not to use in multicore parallel processing. See \code{\link[parallelly]{availableCores}}.}
  \item{...}{All remaining arguments that need to be passed on to \code{\link{get_elr1_mc}} and \code{\link{get_elr1}}: \code{wt_transfer}, \code{wt_transfer_fixed}, \code{ddref}, \code{ddref_fixed}, \code{lat_t0}, \code{lat_eta}, \code{lat_method}, \code{lat_fixed}, \code{alpha}, \code{cancer}, \code{n_sim}, \code{risk_model}, \code{risk_model_mort}, \code{d_base_cancer}, \code{d_base_cancer_mort}, \code{d_base_mort}, \code{age_max}, \code{metric}.}
}
\value{
A data frame with variables \code{sex}, \code{pop}, \code{pop_ref}, \code{mean_rsk}, \code{median_rsk}, \code{CIlo_rsk}, \code{CIup_rsk}, \code{mean_abs}, \code{median_abs}, \code{CIlo_abs}, \code{CIup_abs}, \code{mean_ref}, \code{median_ref}, \code{CIlo_ref}, \code{CIup_ref}. Here, variables \code{_rsk} report the risk, \code{_abs} the absolute number of expected cases given the population defined in \code{x}, and \code{_ref} the absolute number of cases per defined reference population size \code{pop_ref}.
}
\details{
CAVE: All calculations preliminary, not yet validated!

See \code{\link{get_elr1_mc}} for the implementation of Monte Carlo methods for estimating the uncertainty of the lifetime risk estimate.
}
\references{
.
}
\seealso{
\code{\link{get_elr1}},
\code{\link{get_elr1_mc}},
\code{\link{gen_exposure}},
\code{\link{d_pop_ger_fedstate_2024L}}
}
\examples{
## ERR / EAR risk models for solid cancer incidence and mortality
rm_incid <- rm_solid_incid_walsh2021()
rm_mort  <- rm_solid_mort_sumray()

## generate list of 2 exposure events in consecutive years
expo_event <- gen_exposure(n     =2,
                           timing=c(0, 1),
                           distr =c("normal", "triangular"),
                           param =list(c(mean=0.5, sd=0.05),
                                       c(mode=0.5, min=0.1, max=0.6)))

## risk estimates for exposure of the population of Germany
get_elr_pop(d_pop_ger_country_2024L,
            exposure          =expo_event,
            wt_transfer       =c(ERR=0.5, EAR=0.5),
            wt_transfer_fixed =FALSE,
            ddref             =1,
            ddref_fixed       =TRUE,
            lat_t0            =5,
            lat_eta           =6.67,
            lat_method        ="ProZES",
            lat_fixed         =FALSE,
            n_sim             =100L,
            ##
            risk_model        =rm_incid,
            risk_model_mort   =rm_mort,
            d_base_cancer     =d_cancer_ger_incid_solidW_i,
            d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
            d_base_mort       =d_lifetable_ger_2024W,
            metric            ="LEAR",
            age_max           =95,
            ##
            multicore         =FALSE,
            n_cores_max       =10L,
            n_cores_omit      =2L,
            stratify_sex      =TRUE,
            pop_ref           =100000)
}
