\name{rm_leuk_incid_walsh2021}
\alias{rm_leuk_incid_walsh2021}
\title{Risk model for all solid cancer incidence (Walsh et al. 2019)}
\description{
Get the risk model for leukemia incidence due to radiation exposure from Walsh et al. 2019.
}
\usage{
rm_leuk_incid_walsh2021()
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
Walsh et al. A bespoke health risk assessment methodology for the radiation protection of astronauts. Radiat Environ Biophys 2019. DOI: 10.1007/s00411-021-00910-0. Table 4, 7, 8.
}
\seealso{
\code{\link{get_lear1}},
\code{\link{get_lear_pop}},
\code{\link{rm_solid_incid_walsh2021}},
\code{\link{rm_breast_incid_walsh2021}},
\code{\link{rm_solid_incid_sumray}},
\code{\link{rm_solid_mort_sumray}}
}
\examples{
rm_li <- rm_leuk_incid_walsh2021()
rm_li
}
