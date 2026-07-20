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
\item{probs}{Three ordered probabilities used for the lower interval, median, and upper interval.}
}
\value{
A \code{lator_future_prediction} containing pointwise uncertainty bands, the
selected regimen, linked assessment and endpoint evaluation.
}
\description{
Regimen comparison already simulates every feasible candidate under posterior
uncertainty. This function promotes one candidate trajectory into a separate,
auditable forecast artifact without repeating the expensive simulation or
treating the highest-ranked row as an automatic dosing decision.
}
