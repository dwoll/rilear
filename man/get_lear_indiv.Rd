\name{get_lear_indiv}
\alias{get_lear_indiv}
\title{Lifetime excess absolute risk for a single exposed person without considering uncertainty}
\description{
Calculate the projected lifetime excess absolute cancer risk for a single person with radiation exposure. Supports multiple exposure events and multiple cancer outcomes. Calculates risk metrics LAR / LEAR / CER, REID / REIC, ELR, and RADS. Low-level function for a calculation based on a single set of parameters, i.e., without considering uncertainty.
}
\usage{
get_lear_indiv(l_param, risk_model, risk_model_mort,
               d_base_cancer, d_base_cancer_mort,
               d_base_mort, age_max,
               lat_method = c("ProZES", "RadRAT"),
               metric = c("LAR", "LEAR", "CER",
                          "REID", "REIC", "ELR", "RADS"))
}
\arguments{
  \item{l_param}{A list of parameters associated with the exposure event(s) and cancer site(s). See \code{\link{gen_param_mc}} for convenience function to define. Components \code{exposure}, and \code{cancer_site}.
    \itemize{
      \item \code{exposure}: See \code{\link{gen_exposure}} for convenience function to define. A list with one component for each exposure event. Each exposure event component a list with the following components:
      \itemize{
        \item \code{sex}: Character, \code{"f"} or \code{"m"} - must be constant over exposure events.
        \item \code{agex}: Age at exposure.
        \item \code{dose}: In Gy or Sv.
        \item \code{ddref}: Dose- and Dose-Rate Effectiveness Factor with a default of 1.
        \item \code{dose_rate}: Character, \code{"acute"} or \code{"chronic"}.
        \item \code{cancer_site}: Character, one or multiple names of relevant cancer sites, corresponding to exposed organs. See Details.
      }
      \item \code{cancer_site}: A list with one component for each relevant cancer site, corresponding to exposed organs.
      \itemize{
        \item \code{cancer_site}: Character. The name of the cancer site. See Details.
        \item \code{param_err}: Parameter vector for the ERR risk model defined in \code{risk_model} for this cancer site.
        \item \code{param_ear}: Parameter vector for the EAR risk model defined in \code{risk_model} for this cancer site.
        \item \code{param_err_mort}: Only necessary for \code{metric} \code{"REID"} / \code{"REIC"}, \code{"ELR"}. Parameter vector for the ERR mortality risk model defined in \code{risk_model_mort} for this cancer site.
        \item \code{param_ear_mort}: Only necessary for \code{metric} \code{"REID"} / \code{"REIC"}, \code{"ELR"}. Parameter vector for the EAR mortality risk model defined in \code{risk_model_mort} for this cancer site.
        \item \code{wt_transfer}: A named 2-vector giving the ERR / EAR weights used for risk transfer. This vector must have names \code{"ERR"} and \code{"EAR"}, and weights must be within [0, 1] as well as sum to 1.
        \item \code{lat_t0}: Parameter for the S-shaped latency function - central value.
        \item \code{lat_eta}: Parameter for the S-shaped latency function - width of the transition period from 0 to full risk if argument \code{lat_method} is set to \code{"ProZES"}.
      }
    }
  }

  \item{risk_model}{A named list with one component for each cancer site. Definition of ERR and EAR risk models. Cancer sites must correspond to \code{l_param[["cancer_site"]]} and components of \code{base_cancer}. For an example of a risk model for a single cancer site, see \code{\link{rm_solid_incid_walsh2021}}.}
  \item{risk_model_mort}{Only necessary for \code{metric} \code{"REID"} / \code{"REIC"}, \code{"ELR"}. A named list like \code{risk_model} defining ERR and EAR risk models for cancer mortality. For an example, see \code{\link{rm_solid_mort_sumray}}.}
  \item{base_cancer}{A named list with one component for each cancer site. Each component a data frames in wide format defining the sex-specific baseline cancer rate per 1 PY for each year of attained age. Must include variable \code{age_n}, \code{rate_f}, and \code{rate_m}. For an example, see \code{\link{d_cancer_ger_incid_solidW_i}}.}
  \item{base_cancer_mort}{Only necessary for \code{metric} \code{"REID"} / \code{"REIC"}, \code{"ELR"}. A named list like \code{base_cancer} but with cancer mortality rates. For an example, see \code{\link{d_cancer_ger_mort_solidW_i}}.}
  \item{d_base_mort}{Data frame in wide format defining the sex-specific baseline force of mortality for each year of attained age. Must include variable \code{age_n} as well as either \code{q_f} (female), \code{q_m} (male) (as is typical for a lifetable) or \code{rate_f}, \code{rate_m} with the sex-specific mortality rate for 1 PY. For an example, see \code{\link{d_lifetable_ger_2024W}} and \code{\link{d_mort_rate_ger_country_2024W_i}}.}
  \item{age_max}{Sum excess risk up to including this attained age.}
  \item{lat_method}{Method for latency function, either \code{"ProZES"} or \code{"RadRAT"}. See References.}
  \item{metric}{One or several risk metrics. See Details.}
}
\value{
A data frame with one row per cancer site and one column per risk metric. For cancer site \code{total}, all risks from individual cancer sites are summed.
}
\details{
CAVE: All calculations preliminary, not yet validated!

TODO: List cancer sites.

All risk metrics are defined in Sommer et al. 2025:

\itemize{
\item \code{"LAR"} = \code{"LEAR"} = \code{"CER"}: Lifetime Excess Absolute Risk / Cumulative Excess Risk. Calculated assuming that overall survival of the exposed is practically the same as baseline survival. Hence, \code{risk_model_mort}, \code{param_err_mort}, \code{param_ear_mort}, and \code{base_cancer_mort} need not be specified. Plausible for low-dose exposures.
\item \code{"REID"} = \code{"REIC"}: Risk of Exposure-Induced Death / Cancer (depending on the provided cancer risk model and baseline cancer rates). Calculated assuming that overall survival of the exposed can be different than baseline survival. Hence, \code{risk_model_mort}, \code{param_err_mort}, \code{param_ear_mort}, and \code{base_cancer_mort} must be specified.
\item \code{"ELR"}: Excess Lifetime Risk. Calculated assuming that overall survival of the exposed can be different than baseline survival. Hence, \code{risk_model_mort}, \code{param_err_mort}, \code{param_ear_mort}, and \code{base_cancer_mort} must be specified.
\item \code{"RADS"}: Radiation Associated Decrease in Survival. Agnostic to baseline survival.
}

}
\references{
Berrington de Gonzales et al. RadRAT: a radiation risk assessment tool for lifetime cancer risk projection. J Radiat Prot 2012. DOI 10.1088/0952-4746/32/3/205.

Sasaki et al. SUMRAY: R and Python Codes for Calculating Cancer Risk Due to Radiation Exposure of a Population. J Radiat Prot Res 2023 DOI: 10.14407/jrpr.2022.00213.

Sommer et al. Lifetime risks for lung cancer due to occupational radon exposure: A systematic analysis of estimation components. Radiat Res 2025. DOI: 10.1667/RADE-24-00060.1.

Ulanowski et al. On prognostic estimates of radiation risk in medicine and radiation protection. Radiat Environ Biophys 2019. DOI 10.1007/s00411-019-00794-1.

Ulanowski et al. ProZES: the methodology and software tool for assessment of assigned share of radiation in probability of cancer occurrence. Radiat Environ Biophys 2020. DOI 10.1007/s00411-020-00866-7.
}
\seealso{
\code{\link{gen_param_mc}},
\code{\link{gen_exposure}},
\code{\link{get_lear_pop}},
\code{\link{rm_solid_incid_walsh2021}},
\code{\link{d_cancer_ger_incid_solidW_i}},
\code{\link{d_cancer_ger_mort_solidW_i}},
\code{\link{d_lifetable_ger_2024W}},
\code{\link{d_mort_rate_ger_country_2024W_i}}
}
\examples{
rm <- list(all_solid =rm_solid_incid_walsh2021(),
           breast    =rm_breast_incid_walsh2021(),
           leuk_lymph=rm_leuk_incid_walsh2021())

## TODO
## mortality risk model for breast and leuk_lymph
rm_mort <- list(all_solid =rm_solid_mort_sumray(),
                breast    =rm_breast_incid_walsh2021(),
                leuk_lymph=rm_leuk_incid_walsh2021())

base_cancer <- list(all_solid =d_cancer_ger_incid_solidW_i,
                    breast    =d_cancer_ger_incid_breastW_i,
                    leuk_lymph=d_cancer_ger_incid_leuk_lymphW_i)

base_cancer_mort <- list(all_solid =d_cancer_ger_mort_solidW_i,
                         breast    =d_cancer_ger_mort_breastW_i,
                         leuk_lymph=d_cancer_ger_mort_leuk_lymphW_i)

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
                     risk_model       =rm,
                     risk_model_mort  =rm_mort)

get_lear_indiv(l_param         =p_mc[[1]],
               risk_model      =rm,
               risk_model_mort =rm_mort,
               base_cancer     =base_cancer,
               base_cancer_mort=base_cancer_mort,
               d_base_mort     =d_lifetable_ger_2024W,
               age_max         =90,
               lat_method      ="ProZES",
               metric          =c("LEAR", "RADS", "ELR"))
}
