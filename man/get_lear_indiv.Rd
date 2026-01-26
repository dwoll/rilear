\name{get_lear_indiv}
\alias{get_lear_indiv}
\title{Lifetime excess absolute risk for a single exposed person without considering uncertainty}
\description{
Calculate the lifetime excess absolute cancer risk for a single person with radiation exposure. Supports multiple exposure events. Calculates risk metrics LAR / LEAR / CER, REID / REIC, ELR, and RADS. Low-level function for a calculation without considering uncertainty.
}
\usage{
get_lear_indiv(l_param, sex = c("f", "m"), risk_model, risk_model_mort,
               d_base_cancer, d_base_cancer_mort, d_base_mort, age_max,
               lat_method = c("ProZES", "RadRAT"),
               metric = c("LAR", "LEAR", "CER",
                          "REID", "REIC", "ELR", "RADS"))
}
\arguments{
  \item{l_param}{A list of parameters that are associated with uncertainty, including components \code{exposure}, \code{param_err}, \code{param_ear}, \code{param_err_mort}, \code{param_ear_mort}, \code{wt_transfer}, \code{lat_t0}, \code{lat_eta}. Component \code{exposure} needs to be a list with three vectors of the same length: \code{agex} for the age at each exposure event, \code{dose} for the corresponding dose at each exposure event - given in Gy or Sv, and \code{ddref} for the Dose- and Dose-Rate Effectiveness Factor with a default of 1. Components \code{param_err} and \code{param_ear} each are a parameter vector for the ERR / EAR risk models defined in \code{risk_model}. Likewise, components \code{param_err_mort} and \code{param_ear_mort} each are a parameter vector for the ERR / EAR risk models defined in \code{risk_model_mort}. Component \code{wt_transfer} is a 2-vector giving the ERR / EAR weights used for risk transfer. This vector must have names \code{"ERR"} and \code{"EAR"}, and weights must be within [0, 1] as well as sum to 1. Component \code{ddref} defines the dose- and dose-rate effectiveness factor (DDREF) with a default of 1. Components \code{lat_t0} and \code{lat_eta} are used for the S-shaped latency function with \code{t0} being the central value and \code{lat_eta} defining the width of the transition period from 0 to full risk if argument \code{lat_method} is set to \code{"ProZES"}.}
  \item{sex}{Value from a factor, either \code{"f"} or \code{"m"}.}
  \item{risk_model}{A list defining ERR and EAR risk models for cancer for the same type of cancer as \code{d_base_cancer}. For an example, see \code{\link{rm_solid_incid_walsh2021}}.}
  \item{risk_model_mort}{Only necessary for \code{metric} \code{"REID"} / \code{"REIC"}, \code{"ELR"}. A list defining ERR and EAR risk models for cancer mortality for the same type of cancer as \code{d_base_cancer_mort}. For an example, see \code{\link{rm_solid_mort_sumray}}.}
  \item{d_base_cancer}{Data frame in wide format defining the sex-specific baseline cancer rate per 1 PY for each year of attained age. Must include variable \code{age_n}, \code{rate_f}, and \code{rate_m}. For an example, see \code{\link{d_cancer_ger_incid_solidW_i}}.}
  \item{d_base_cancer_mort}{Only necessary for \code{metric} \code{"REID"} / \code{"REIC"}, \code{"ELR"}. Data frame in wide format defining the sex-specific baseline cancer mortality rate per 1 PY for each year of attained age for the same cancer as in \code{d_base_cancer}. Must include variable \code{age_n}, \code{rate_f}, and \code{rate_m}. For an example, see \code{\link{d_cancer_ger_mort_solidW_i}}.}
  \item{d_base_mort}{Data frame in wide format defining the sex-specific baseline force of mortality for each year of attained age. Must include variable \code{age_n} as well as either \code{q_f} (female), \code{q_m} (male) (as is typical for a lifetable) or \code{rate_f}, \code{rate_m} with the sex-specific mortality rate for 1 PY. For an example, see \code{\link{d_lifetable_ger_2024W}} and \code{\link{d_mort_rate_ger_country_2024W_i}}.}
  \item{age_max}{Sum excess risk up to including this attained age.}
  \item{lat_method}{Latency function, either \code{"ProZES"} or \code{"RadRAT"}. See References.}
  \item{metric}{One or several risk metrics. See Details.}
}
\value{
A named vector with one value for each metric giving the predicted excess lifetime risk.
}
\details{
CAVE: All calculations preliminary, not yet validated!

All risk metrics are defined in Sommer et al. 2025:

\itemize{
\item \code{"LAR"} = \code{"LEAR"} = \code{"CER"}: Lifetime Excess Absolute Risk / Cumulative Excess Risk. Calculated assuming that overall survival of the exposed is practically the same as baseline survival. Hence, \code{risk_model_mort}, \code{param_err_mort}, \code{param_ear_mort}, and \code{d_base_cancer_mort} need not be specified. Plausible for low-dose exposures.
\item \code{"REID"} = \code{"REIC"}: Risk of Exposure-Induced Death / Cancer (depending on the provided cancer risk model and baseline cancer rates). Calculated assuming that overall survival of the exposed can be different than baseline survival. Hence, \code{risk_model_mort}, \code{param_err_mort}, \code{param_ear_mort}, and \code{d_base_cancer_mort} must be specified.
\item \code{"ELR"}: Excess Lifetime Risk. Calculated assuming that overall survival of the exposed can be different than baseline survival. Hence, \code{risk_model_mort}, \code{param_err_mort}, \code{param_ear_mort}, and \code{d_base_cancer_mort} must be specified.
\item \code{"RADS"}: Radiation Associated Decrease in Survival. Agnostic to baseline survival.
}

}
\references{
Berrington de Gonzales et al. RadRAT: a radiation risk assessment tool for lifetime cancer risk projection. J Radiat Prot 2012. DOI 10.1088/0952-4746/32/3/205.

Sasaki et al. SUMRAY: R and Python Codes for Calculating Cancer Risk Due to Radiation Exposure of a Population. J Radiat Prot Res 2023 DOI: 10.14407/jrpr.2022.00213.

Sommer et al. Lifetime risks for lung cancer due to occupational radon exposure: A systematic analysis of estimation components. Radiat Res 2025. DOI: 10.1667/RADE-24-00060.1.

Ulanowski et al. ProZES: the methodology and software tool for assessment of assigned share of radiation in probability of cancer occurrence. Radiat Environ Biophys 2020. DOI 10.1007/s00411-020-00866-7.
}
\seealso{
\code{\link{get_lear_pop}},
\code{\link{rm_solid_incid_walsh2021}},
\code{\link{d_cancer_ger_incid_solidW_i}},
\code{\link{d_cancer_ger_mort_solidW_i}},
\code{\link{d_lifetable_ger_2024W}},
\code{\link{d_mort_rate_ger_country_2024W_i}}
}
\examples{
## ERR / EAR risk models for solid cancer incidence and mortality
rm_incid <- rm_solid_incid_walsh2021()
rm_mort  <- rm_solid_mort_sumray()

## single point estimate after two exposures at age 20, 21
get_lear_indiv(l_param=list(exposure      =list(dose=c(0.5, 0.7),
                                                agex=c(20, 21),
                                                ddref=c(1, 1)),
                            param_err     =rm_incid$err$param,
                            param_ear     =rm_incid$ear$param,
                            param_err_mort=rm_mort$err$param,
                            param_ear_mort=rm_mort$ear$param,
                            wt_transfer   =c(ERR=0.5, EAR=0.5),
                            lat_t0        =5,
                            lat_eta       =6.25),
               sex               ="f",
               risk_model        =rm_incid,
               risk_model_mort   =rm_mort,
               d_base_cancer     =d_cancer_ger_incid_solidW_i,
               d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
               d_base_mort       =d_lifetable_ger_2024W,
               age_max           =95,
               lat_method        ="ProZES",
               metric            =c("LEAR", "RADS"))
}
