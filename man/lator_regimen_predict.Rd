\name{lator_regimen_predict}
\alias{lator_regimen_predict}
\title{Create an explicit future prediction for a selected regimen}
\usage{
lator_regimen_predict(
  comparison,
  candidate_id,
  probs = c(0.05, 0.5, 0.95)
)
}
\arguments{
\item{comparison}{Result from \code{lator_regimen_optimise()}.}
\item{candidate_id}{Candidate deliberately selected by the user.}
\item{probs}{Three ordered posterior probabilities used for the lower interval,
median, and upper interval. The interval reflects uncertainty in the
individual's ETA posterior; it excludes residual measurement variability and
is not a confidence interval for a population mean.}
}
\value{
A \code{lator_future_prediction} containing pointwise posterior prediction
intervals, their uncertainty definition, the selected regimen, linked
assessment and endpoint evaluation, and native-unit numerical posterior
outcomes for every endpoint component.
}
\description{
Regimen comparison already simulates every feasible candidate under posterior
uncertainty. This function promotes one candidate trajectory into a separate,
auditable forecast artifact without repeating the expensive simulation or
treating the highest-ranked row as an automatic dosing decision.
}
