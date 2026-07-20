## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")


## ----example------------------------------------------------------------------
library(LibeRator)
example <- lator_example_aed()
example$patient

lator_covariate_at(
  example$patient, "WT", times = c(0, 168, 180),
  method = "locf", max_age = 365 * 24
)


## ----assessment---------------------------------------------------------------
static <- lator_assess(
  example$patient, example$model, example$endpoint,
  covariate_policies = list(WT = list(method = "locf", max_age = 365 * 24))
)

dynamic <- lator_assess(
  example$patient, example$model, example$endpoint,
  mode = "dynamic", process_scale = 0.1,
  covariate_policies = list(WT = list(method = "locf", max_age = 365 * 24))
)

dynamic$eta_trajectory


## ----regimen, eval=FALSE------------------------------------------------------
# candidates <- lator_regimen_candidates(
#   amounts = c(100, 200, 300), intervals = c(12, 24), horizon = 168
# )
# comparison <- lator_regimen_optimise(
#   dynamic, example$patient, candidates,
#   endpoint = example$endpoint, nsim = 500, seed = 42
# )
# comparison$summary
# 
# # Promote a deliberately selected candidate to a separate forecast artifact.
# selected <- comparison$summary$candidate_id[[1L]]
# forecast <- lator_regimen_predict(comparison, selected)
# forecast$forecast


## ----gui, eval=FALSE----------------------------------------------------------
# workspace <- lator_workspace(
#   "~/LibeR/liberator-workspace",
#   passphrase = "use a long, unique research passphrase"
# )
# lator_patient_save(workspace, example$patient)
# lator_gui(
#   workspace = workspace,
#   models = list(aed = example$model),
#   endpoints = list(aed = example$endpoint)
# )

