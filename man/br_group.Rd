% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benefit_risk.R
\name{br_group}
\alias{br_group}
\title{Posterior Samples for a Benefit/Risk Group}
\usage{
br_group(label, ...)
}
\arguments{
\item{label}{a string indicating the name of the group.}

\item{...}{named arguments which correspond to the names of the
benefits/risks specified by \code{benefit()} and \code{risk()} in a call to \code{br()}.}
}
\value{
A named list with the posterior samples and an assigned S3 class.
}
\description{
Posterior Samples for a Benefit/Risk Group
}
\details{
This function is intended to be used as an input argument to
the \code{br()} function.
}
\examples{
set.seed(1132)
out <- br(
  benefit("CV", function(x) x, weight = 1),
  risk("DVT", function(x) - .5 * x, weight = 1),

  br_group(
    label = "PBO",
    CV = rnorm(1e4, .1),
    DVT = rnorm(1e4, .1)
  ),
  br_group(
    label = "TRT",
    CV = rnorm(1e4, 2),
    DVT = rnorm(1e4, 1)
  )
)

out

summary(out, probs = c(.025, .5, .975))
summary(out, reference = "PBO")

plot(out)
# adjusted relative to PBO
plot(out, reference = "PBO")

plot_utility(out)
plot_utility(out, reference = "PBO")
plot_utility(out, stacked = TRUE)
}
