\name{run_gui}
\alias{run_gui}
\title{Open web-based GUI in browser}
\description{
Opens the web-based GUI in an external browser. Implemented using package shiny.
}
\usage{
run_gui(...)
}
\arguments{
  \item{...}{Arguments passed to \code{\link[shiny]{runApp}} from package \code{shiny}. Supply \code{port=80} if a web browser refuses to connect to the randomly chosen port for security reasons.}
}
\details{
This function is a wrapper for \code{\link[shiny]{runApp}} from package \code{shiny} which runs the included rilear application. See \code{vignette("gui", package="rilear")} for documentation.
}
\seealso{
\code{\link[shiny]{runApp}}
}
\examples{
\dontrun{
run_gui()
}
}
