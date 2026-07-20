test_that("static and dynamic assessments use the C++ individual objective", {
  endpoint <- lator_endpoint_aed("Drug A", 1, 4, "mg/L", "teaching source")
  static <- lator_assess(lator_test_patient(), lator_test_model(), endpoint)
  expect_s3_class(static, "lator_assessment")
  expect_equal(static$convergence, 0L)
  expect_equal(nrow(static$eta_trajectory), 1L)
  expect_true(is.finite(static$diagnostics$gradient_max))

  dynamic <- lator_assess(lator_test_patient(boundary = TRUE), lator_test_model(), endpoint,
                          mode = "dynamic", process_scale = 0.2)
  expect_equal(length(dynamic$eta), 2L)
  expect_equal(unique(dynamic$eta_trajectory$occasion), 1:2)
  expect_true(any(dynamic$data$.LATOR_ROLE == "prechange"))
  expect_true(any(dynamic$data$.LATOR_ROLE == "postchange"))
})

test_that("unresolved required covariates stop rather than being invented", {
  endpoint <- lator_endpoint_aed("Drug A", 1, 4, "mg/L", "teaching source")
  expect_error(lator_assess(lator_test_patient(), lator_test_model(TRUE), endpoint), "remain unresolved")
  fitted <- lator_assess(
    lator_test_patient(), lator_test_model(TRUE), endpoint,
    covariate_policies = list(WT = list(fallback = 70, fallback_unit = "kg"))
  )
  expect_true(any(grepl("explicit fallback", fitted$warnings)))
})

test_that("regimen comparison is ranked under posterior uncertainty", {
  patient <- lator_test_patient()
  endpoint <- lator_endpoint_aed("Drug A", 1, 4, "mg/L", "teaching source")
  assessment <- lator_assess(patient, lator_test_model(), endpoint)
  candidates <- lator_regimen_candidates(c(50, 100), c(12, 24), horizon = 24)
  compared <- lator_regimen_optimise(assessment, patient, candidates, nsim = 4, grid_step = 2, seed = 1)
  expect_s3_class(compared, "lator_regimen_comparison")
  expect_equal(nrow(compared$summary), 4L)
  expect_true(all(diff(compared$summary$objective) >= 0))

  selected <- compared$summary$candidate_id[[1L]]
  forecast <- lator_regimen_predict(compared, selected)
  expect_s3_class(forecast, "lator_future_prediction")
  expect_identical(forecast$candidate_id, selected)
  expect_identical(forecast$assessment_id, assessment$assessment_id)
  expect_true(nrow(forecast$forecast) > 1L)
  expect_true(all(c("time", "lower", "median", "upper", "draws") %in% names(forecast$forecast)))
  expect_true(all(forecast$forecast$lower <= forecast$forecast$median))
  expect_true(all(forecast$forecast$median <= forecast$forecast$upper))
})
