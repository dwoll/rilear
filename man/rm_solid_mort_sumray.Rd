\name{rm_solid_mort_sumray}
\alias{rm_solid_mort_sumray}
\title{Risk model for all solid cancer mortality (Sasaki et al. 2023)}
\description{
Get the risk model for all solid cancer mortality due to radiation exposure from Sasaki et al. 2023 for the SUMRAY package.
}
\usage{
rm_solid_mort_sumray()
}
\arguments{
}
\value{
A list with these components:

\itemize{
\item \code{err}: A list defining the ERR risk model. Components \code{param}, \code{varm}, and \code{f}. \code{param}: A vector with estimated parameter values (= maximum likelihood coefficient estimates) for the ERR model function \code{f}. \code{varm}: The estimated covariance matrix of paramter estimates in \code{param}.
\item \code{ear}: A list defining the EAR risk model in the same way as the ERR risk  model.
}

}
\references{
Sasaki et al. SUMRAY: R and Python Codes for Calculating Cancer Risk Due to Radiation Exposure of a Population. J Radiat Prot Res 2023 DOI: 10.14407/jrpr.2022.00213.

\url{https://github.com/JapanHealthPhysicsSociety/SUMRAY/blob/main/R/R/RiskModel_LSSAllSolidCancerMortality.r}
}
\seealso{
\code{\link{get_lear1}},
\code{\link{get_lear_pop}},
\code{\link{rm_solid_incid_walsh2021}},
\code{\link{rm_breast_incid_walsh2021}},
\code{\link{rm_leuk_incid_walsh2021}},
\code{\link{rm_solid_incid_sumray}}
}
\examples{
rm_scm <- rm_solid_mort_sumray()
rm_scm
}
