\name{get_elr1_mc}
\alias{get_elr1_mc}
\title{Lifetime excess absolute risk for a single exposed person considering uncertainty}
\description{
Calculate the lifetime excess absolute cancer risk for a single person with radiation exposure. Supports multiple exposure events. Calculates LAR / LEAR / CER, REID / REIC, ELR, and RADS. Uses Monte Carlo methods to take into account the uncertainty in several relevant parameters.
}
\usage{
get_elr1_mc(exposure, wt_transfer, wt_transfer_fixed = FALSE,
            ddref = 1, ddref_fixed = FALSE, lat_t0, lat_eta,
            lat_method = c("ProZES", "RadRAT"), lat_fixed = FALSE,
            alpha = 0.05, n_sim = 1000L, multicore = FALSE,
            n_cores_max = 10L, n_cores_omit = 2L, aggr_mc = TRUE, ...)
}
\arguments{
  \item{exposure}{A list defining the exposure events with one component for each such event. Each component defining a single exposure event is a list with components \code{agex} for the age at exposure, \code{distr} for the dose distribution class, and \code{param} with the parameters necessary to specify the dose distribution in Gy or Sv. See \code{\link{gen_exposure}} - a utility function that may be used to create such a list.}
  \item{wt_transfer}{A 2-vector giving the ERR / EAR weights used for risk transfer. This vector must have names \code{"ERR"} and \code{"EAR"}, and weights must be within [0, 1] as well as sum to 1.}
  \item{wt_transfer_fixed}{Should indicated ERR / EAR weights in \code{wt_transfer} be treated without uncertainty?}
  \item{ddref}{The dose- and dose-rate effectiveness factor (DDREF) with a default of 1.}
  \item{ddref_fixed}{Should indicated DDREF value in \code{ddref} be treated without uncertainty?}
  \item{lat_t0}{Parameter for the S-shaped latency function: The central value in years.}
  \item{lat_eta}{Parameter for the S-shaped latency function if argument \code{lat_method} is set to \code{"ProZES"}: The width of the transition period from 0 to full risk in years.}
  \item{lat_method}{Method for defining the S-shaped latency function, either \code{"ProZES"} or \code{"RadRAT"}. See References.}
  \item{lat_fixed}{Should indicated latency parameters in \code{lat_t0} and \code{lat_eta} be treated without uncertainty?}
  \item{alpha}{Statistical significance level defining the width of the confidence interval as 1 - alpha.}
  \item{n_sim}{Number of Monte Carlo simulations.}
  \item{multicore}{Do multicore parallel processing?}
  \item{n_cores_max}{Maximum number of cores to use in multicore processing. See \code{\link[parallelly]{availableCores}}.}
  \item{n_cores_omit}{Number of cores not to use in multicore parallel processing. See \code{\link[parallelly]{availableCores}}.}
  \item{aggr_mc}{Aggregate results from all Monte Carlo simulation runs  by summarizing them to the mean, median, and confidence interval?}
  \item{...}{All remaining arguments without uncertainty that need to be passed on to \code{\link{get_elr1}}: \code{sex}, \code{risk_model}, \code{risk_model_mort}, \code{d_base_cancer}, \code{d_base_cancer_mort}, \code{d_base_mort}, \code{age_max}, \code{metric}.}
}
\value{
For \code{aggr_mc=TRUE}, a matrix with one column per metric, and one row each for the mean, median, and confidence interval boundaries for the excess lifetime risk values. For \code{aggr_mc=FALSE}, a matrix with one column per metric and \code{n_sim} rows with the individual results from the Monte Carlo simulation runs.
}
\details{
CAVE: All calculations preliminary, not yet validated!

Uncertainty is taken into account by independently simulating values for parameters \code{exposure} (dose), \code{param_err}, \code{param_ear}, \code{param_err_mort}, \code{param_ear_mort}, \code{wt_transfer}, \code{ddref}, \code{lat_t0}, \code{lat_eta} passed on to \code{\link{get_elr1}}. For each set of simulated parameters, a value for the lifetime excess absolute lifetime risk is calculated. These methods are used:

\itemize{
\item Dose: Random samples from the distribution specified in \code{exposure$distr} together with \code{exposure$param}.
\item Risk model parameters: Random samples from a multivariate normal distribution with the mean and covariance matrix are taken from the corresponding \code{risk_model} arguments where these are expected to be stored in components \code{err$param} / \code{ear$param} and \code{err$varm} / \code{ear$varm}, respectively.
\item ERR-EAR transfer weights: Random samples of ERR weights from a beta distribution with mean as indicated in \code{wt_transfer["ERR"]} and variance 0.25 of the corresponding normal distribution.
\item DDREF: Random samples from a beta distribution scaled to [1, 3] with mean 1.5 and variance 0.1 of the corresponding normal distribution.
\item Latency function t0: Random samples from a uniform distribution in the range [0.8*lat_t0, 1.2*lat_t0].
\item Latency function eta: No uncertainty applied.
}

}
\references{
Berrington de Gonzales et al. RadRAT: a radiation risk assessment tool for lifetime cancer risk projection. J Radiat Prot 2012. DOI 10.1088/0952-4746/32/3/205.

Ulanowski et al. ProZES: the methodology and software tool for assessment of assigned share of radiation in probability of cancer occurrence. Radiat Environ Biophys 2020. DOI 10.1007/s00411-020-00866-7.
}
\seealso{
\code{\link{get_elr1}},
\code{\link{get_elr_pop}},
\code{\link{gen_exposure}}
}
\examples{
## ERR / EAR risk models for solid cancer incidence and mortality
rm_incid <- rm_solid_incid_walsh2021()
rm_mort  <- rm_solid_mort_sumray()

## generate list of 2 exposure events
expo_event <- gen_exposure(n    =2,
                           agex =c(20, 21),
                           distr=c("normal", "triangular"),
                           param=list(c(mean=0.5, sd=0.05),
                                      c(mode=0.5, min=0.1, max=0.6)))

get_elr1_mc(## parameters with uncertainty
            exposure          =expo_event,
            wt_transfer       =c(ERR=0.5, EAR=0.5),
            wt_transfer_fixed =FALSE,
            ddref             =1,
            ddref_fixed       =TRUE,
            lat_t0            =5,
            lat_eta           =6.25,
            lat_method        ="ProZES",
            lat_fixed         =FALSE,
            ## parameters without uncertainty
            sex               ="f",
            risk_model        =rm_incid,
            risk_model_mort   =rm_mort,
            d_base_cancer     =d_cancer_ger_incid_solidW_i,
            d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
            d_base_mort       =d_lifetable_ger_2024W,
            alpha             =0.05,
            age_max           =95,
            metric            ="REIC",
            n_sim             =100L,
            multicore         =FALSE,
            n_cores_max       =10L,
            n_cores_omit      =2L,
            aggr_mc           =TRUE)
}
