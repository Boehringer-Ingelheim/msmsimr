% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributions.R
\name{cumulative_hazard}
\alias{cumulative_hazard}
\title{Cumulative Hazard Function}
\usage{
cumulative_hazard(t, dst)
}
\arguments{
\item{t}{non-negative numeric vector of time points}

\item{dst}{a \link{survival-distribution} object}
}
\value{
numeric vector of cumulative hazards evaluated at \code{t}
}
\description{
Cumulative hazard function of a survival distribution.
}
\seealso{
\link{survival-distribution}
}
\concept{survival distribution methods}
