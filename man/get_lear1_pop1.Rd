\name{get_lear1_pop1}
\alias{get_lear1_pop1}
\title{Lifetime excess absolute risk for 1 population subgroup of exposed persons without considering uncertainty}
\description{
Calculate the lifetime excess absolute cancer risk for a single subgroup out of a population with radiation exposure. Supports multiple exposure events. Calculates LAR / LEAR / CER, REID / REIC, ELR, and RADS.
}
\usage{
get_lear1_pop1(x, l_param, age_max, metric, ...)
}
\arguments{
  \item{x}{One observation = one row of a data frame defining the exposed population. Long format with respect to sex. Needs variables \code{age_n} with the numeric age, \code{sex} - a factor with levels \code{"f"} and \code{"m"}, and \code{pop} for the population size. For an example, see \code{\link{d_pop_ger_fedstate_2024L}}.}
  \item{l_param}{A list of parameters describing the exposure event and cancer sites. See \code{\link{get_lear_indiv}}.}
  \item{age_max}{Sum excess risk up to including this attained age.}
  \item{metric}{One or several risk metrics. See \code{\link{get_lear_indiv}}.}
  \item{...}{All remaining arguments that get passed on to \code{\link{get_lear_indiv}}: \code{lat_method}, \code{base_cancer}, \code{base_cancer_mort} (if available), \code{d_base_mort}, \code{risk_model}, \code{risk_model_mort} (if available).}
}
\value{
A data frame with population information from \code{x}, and also \code{site} for the name of the cancer site. Furthermore, one variable for each requested metric for lifetime excess absolute risk.
}
\details{
CAVE: All calculations preliminary, not yet validated!
}
\references{
.
}
\seealso{
\code{\link{gen_exposure}},
\code{\link{get_lear_indiv}},
\code{\link{get_lear_pop}},
\code{\link{get_lear1_pop}},
\code{\link{d_pop_ger_fedstate_2024L}}
}
\examples{
rm <- list(all_solid =rm_solid_incid_walsh2021(),
           breast    =rm_breast_incid_walsh2021(),
           leuk_lymph=rm_leuk_incid_walsh2021())

base_cancer <- list(all_solid =d_cancer_ger_incid_solidW_i,
                    breast    =d_cancer_ger_incid_breastW_i,
                    leuk_lymph=d_cancer_ger_incid_leuk_lymphW_i)

wt_transfer <- list(all_solid =c(ERR=0.5, EAR=0.5),
                    breast    =c(ERR=0.0, EAR=1.0),
                    leuk_lymph=c(ERR=1.0, EAR=0.0))

lat_t0 <- list(all_solid =4,
               breast    =4,
               leuk_lymph=1.5)

lat_eta <- list(all_solid =6.25,
                breast    =6.25,
                leuk_lymph=7.66)

expo_event <- gen_exposure(n          =2,
                           sex        ="f",
                           agex       =c(20, 21),
                           dose_distr =c("fixed", "normal"),
                           dose_param =list(0.5, c(mean=0.3, sd=0.05)),
                           ddref      =c(1, 1.5),
                           dose_rate  =c("acute", "acute"),
                           cancer_site=list("all_solid",
                                            c("all_solid", "leuk_lymph")))

p_mc <- gen_param_mc(exposure         =expo_event,
                     n_sim            =5L,
                     wt_transfer      =wt_transfer,
                     lat_t0           =lat_t0,
                     lat_eta          =lat_eta,
                     lat_method       ="ProZES",
                     lat_fixed        =FALSE,
                     wt_transfer_fixed=FALSE,
                     ddref_fixed      =FALSE,
                     risk_model       =rm)

## solid cancer risk estimates
## for exposure of the population of Germany
get_lear1_pop1(x          =d_pop_ger_country_2024L[37, ],
               l_param    =p_mc[[1]],
               lat_method ="ProZES",
               risk_model =rm,
               base_cancer=base_cancer,
               d_base_mort=d_lifetable_ger_2024W,
               metric     =c("LEAR", "RADS"),
               age_max    =90)
}
