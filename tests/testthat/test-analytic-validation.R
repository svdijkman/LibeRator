test_that("an analytic virtual patient is recovered and forecast end-to-end", {
  true_eta <- log(1.4)
  dose <- 120
  concentration <- function(time) 4 * exp(-3 * exp(true_eta) / 30 * time)
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT", "CMT", "DV", "MDV"),
    ADVAN = 1, TRANS = 2,
    PRED = "CL=THETA(1)*exp(ETA(1));V=THETA(2);S1=V",
    ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(THETA = 1:2, Value = c(3, 30), FIX = TRUE),
    OMEGAS = data.frame(OMEGA = 1, Value = 0.25, FIX = TRUE),
    SIGMAS = data.frame(SIGMA = 1, Value = 1e-6, FIX = TRUE)
  )
  patient <- lator_patient_new("VP-TEST")
  patient <- lator_patient_add_event(patient, "dose", 0, "Virtual drug", dose, "mg")
  for (time in c(1, 3, 6, 12)) {
    patient <- lator_patient_add_event(
      patient, "concentration", time, "Virtual drug", concentration(time), "mg/L"
    )
  }
  endpoint <- lator_endpoint_aed(
    "Virtual drug", 1.5, 3.5, "mg/L", "Analytic test target"
  )
  assessment <- lator_assess(
    patient, model, endpoint, maxit = 150, tolerance = 1e-10
  )
  expect_equal(assessment$convergence, 0L)
  expect_equal(assessment$eta[[1L]], true_eta, tolerance = 0.01)

  comparison <- lator_regimen_optimise(
    assessment, patient,
    lator_regimen_candidates(c(60, 120, 240), 24, horizon = 24),
    endpoint = endpoint, nsim = 16, grid_step = 2, seed = 20260723
  )
  by_dose <- comparison$summary[order(comparison$summary$amount), ]
  expect_true(all(diff(by_dose$median_metric) > 0))
  forecast <- lator_regimen_predict(comparison, comparison$summary$candidate_id[[1L]])
  expect_s3_class(forecast, "lator_future_prediction")
  expect_gt(nrow(forecast$forecast), 1L)
})
