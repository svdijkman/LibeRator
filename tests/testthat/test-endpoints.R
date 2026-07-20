test_that("AED endpoints target the supplied range midpoint", {
  endpoint <- lator_endpoint_aed("Drug A", 10, 20, "mg/L", "teaching source")
  predictions <- data.frame(TIME = 0:2, IPRED = c(14, 15, 16))
  evaluated <- lator_endpoint_evaluate(endpoint, predictions)
  expect_equal(evaluated$median_metric, 15)
  expect_equal(evaluated$attainment_probability, 1)
  expect_equal(endpoint$rules$target, 15)
})

test_that("beta-lactam endpoint resolves longitudinal MIC", {
  patient <- lator_patient_new("P001")
  patient <- lator_patient_add_event(patient, "covariate", 0, "MIC", 2, "mg/L")
  endpoint <- lator_endpoint_beta_lactam("Example beta-lactam", 0.4, source = "teaching source")
  predictions <- data.frame(TIME = c(0, 1, 2), IPRED = c(4, 2, 0))
  evaluated <- lator_endpoint_evaluate(endpoint, predictions, patient)
  expect_equal(evaluated$median_metric, 0.5, tolerance = 1e-12)
  expect_equal(evaluated$attainment_probability, 1)
})

test_that("ATG endpoints validate explicit pre-event windows", {
  targets <- data.frame(window_start = c(-24, -6), window_end = c(-18, 0),
                        lower = c(1, 2), upper = c(3, 4))
  endpoint <- lator_endpoint_atg("ATG", targets, "AU", "teaching source")
  expect_s3_class(endpoint, "lator_endpoint")
  expect_error(lator_endpoint_atg("ATG", transform(targets, lower = 9), "AU", "x"), "invalid")
})
