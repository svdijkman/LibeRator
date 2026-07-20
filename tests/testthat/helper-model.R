lator_test_model <- function(covariate = FALSE) {
  LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT", "CMT", "DV", "MDV", if (covariate) "WT"),
    ADVAN = 1,
    PRED = paste0(
      "CL=THETA(1)", if (covariate) "*(WT/70)^0.75" else "", "*exp(ETA(1));",
      "V=THETA(2)", if (covariate) "*(WT/70)" else "", ";S1=V"
    ),
    ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(THETA = 1:2, Value = c(3, 30)),
    OMEGAS = data.frame(OMEGA = 1, Value = 0.2),
    SIGMAS = data.frame(SIGMA = 1, Value = 0.4),
    COVARIATES = if (covariate) "WT" else NULL
  )
}

lator_test_patient <- function(boundary = FALSE, covariate = FALSE) {
  patient <- lator_patient_new("STUDY-001", "TEACH", "Patient 001")
  if (covariate) patient <- lator_patient_add_event(patient, "covariate", 0, "WT", 70, "kg")
  patient <- lator_patient_add_event(patient, "dose", 0, "Drug A", 100, "mg")
  patient <- lator_patient_add_event(patient, "concentration", 2, "Drug A", 2.8, "mg/L")
  if (boundary) patient <- lator_patient_add_event(patient, "state_boundary", 6)
  patient <- lator_patient_add_event(patient, "dose", 12, "Drug A", 100, "mg")
  patient <- lator_patient_add_event(patient, "concentration", 14, "Drug A", 2.2, "mg/L")
  patient
}
