#' Synthetic AED teaching case
#'
#' Creates a small, non-clinical longitudinal case containing an ADVAN1 model,
#' intermittent weight measurements, repeat dosing, TDM observations, a latent
#' state boundary, and an illustrative therapeutic-range endpoint. It is meant
#' for software teaching and tests; its target range is not a real dosing rule.
#'
#' @return Named list with `model`, `patient`, and `endpoint`.
#' @export
lator_example_aed <- function() {
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT", "CMT", "DV", "MDV", "WT"),
    ADVAN = 1, TRANS = 2,
    PRED = paste(
      "CL=THETA(1)*(WT/70)^0.75*exp(ETA(1))",
      "V=THETA(2)*(WT/70)*exp(ETA(2))", "S1=V", sep = ";"
    ),
    ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(THETA = 1:2, Value = c(3.5, 42)),
    OMEGAS = data.frame(
      OMEGA = 1:3, Value = c(0.16, 0.04, 0.12),
      ROW = c(1, 2, 2), COL = c(1, 1, 2)
    ),
    SIGMAS = data.frame(SIGMA = 1, Value = 1.5), COVARIATES = "WT",
    LIK_CONFIG = LibeRation::nm_lik_config(omega = "full")
  )
  attr(model, "name") <- "Synthetic AED one-compartment model"
  patient <- lator_patient_new("TEACH-AED-001", "LIBERATOR-TEACH", "Teaching patient")
  events <- list(
    list(type = "covariate", time = 0, name = "WT", value = 68, unit = "kg"),
    list(type = "dose", time = 0, name = "Example AED", value = 300, unit = "mg"),
    list(type = "concentration", time = 2, name = "Example AED", value = 7.8, unit = "mg/L"),
    list(type = "concentration", time = 10, name = "Example AED", value = 3.2, unit = "mg/L"),
    list(type = "state_boundary", time = 168),
    list(type = "covariate", time = 168, name = "WT", value = NA_real_, unit = "kg",
         missing_reason = "not weighed at follow-up"),
    list(type = "dose", time = 168, name = "Example AED", value = 300, unit = "mg"),
    list(type = "concentration", time = 170, name = "Example AED", value = 6.1, unit = "mg/L"),
    list(type = "concentration", time = 178, name = "Example AED", value = 2.9, unit = "mg/L")
  )
  patient <- lator_patient_add_events(patient, events)
  endpoint <- lator_endpoint_aed(
    "Example AED", 2, 8, "mg/L", source = "Synthetic teaching target; not a clinical range"
  )
  list(model = model, patient = patient, endpoint = endpoint)
}
