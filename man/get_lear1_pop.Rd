\name{get_lear1_pop}
\alias{get_lear1_pop}
\title{Lifetime excess absolute risk for a population of exposed persons without considering uncertainty}
\description{
Calculate the projected lifetime excess absolute cancer risk for a population of persons with radiation exposure. Supports multiple exposure events. Calculates LAR / LEAR / CER, REID / REIC, ELR, and RADS.
}
\usage{
get_lear1_pop(l_param, d_pop, ...)
}
\arguments{
  \item{l_param}{A list of parameters describing the exposure event and cancer sites. See \code{\link{get_lear_indiv}}.}
  \item{d_pop}{A data frame defining the exposed population. See \code{\link{get_lear_pop}}.}
  \item{...}{All remaining arguments that get passed on to \code{\link{get_lear_indiv}}: \code{metric}, \code{lat_method}, \code{base_cancer}, \code{base_cancer_mort} (if available), \code{d_base_mort}, \code{risk_model}, \code{risk_model_mort} (if available), \code{age_max}.}
}
\value{
A data frame with population information from \code{d_pop}, and also \code{site} for the name of the cancer site. Furthermore, one variable for each requested metric for lifetime excess absolute risk.
}
\details{
CAVE: All calculations preliminary, not yet validated!
}
\references{
.
}
\seealso{
\code{\link{get_lear_indiv}},
\code{\link{get_lear_pop}},
\code{\link{get_lear1_pop1}},
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
get_lear1_pop(l_param    =p_mc[[1]],
              d_pop      =d_pop_ger_country_2024L,
              lat_method ="ProZES",
              risk_model =rm,
              base_cancer=base_cancer,
              d_base_mort=d_lifetable_ger_2024W,
              metric     =c("LEAR", "RADS"),
              age_max    =90)
}
