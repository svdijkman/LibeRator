test_that("covariate policies preserve age, missingness, and fallback provenance", {
  patient <- lator_patient_new("P001")
  patient <- lator_patient_add_event(patient, "covariate", 0, "WT", 70, "kg")
  patient <- lator_patient_add_event(patient, "covariate", 12, "WT", NA_real_, "kg",
                                     missing_reason = "not weighed at visit")
  resolved <- lator_covariate_at(patient, "WT", c(0, 12, 48), method = "locf", max_age = 24)
  expect_equal(resolved$value[1:2], c(70, 70))
  expect_identical(resolved$status[[2L]], "resolved_after_missing")
  expect_true(resolved$scheduled_missing[[2L]])
  expect_equal(resolved$missing_reason[[2L]], "not weighed at visit")
  expect_identical(resolved$status[[3L]], "stale")

  fallback <- lator_covariate_at(patient, "CRCL", 12, fallback = 90, fallback_unit = "mL/min")
  expect_identical(fallback$status, "fallback")
  expect_identical(fallback$method, "fallback")
})

test_that("linear interpolation is bracketed and unit-safe", {
  patient <- lator_patient_new("P001")
  patient <- lator_patient_add_event(patient, "covariate", 0, "WT", 60, "kg")
  patient <- lator_patient_add_event(patient, "covariate", 10, "WT", 70, "kg")
  expect_equal(lator_covariate_at(patient, "WT", 5, method = "linear")$value, 65)
  expect_true(is.na(lator_covariate_at(patient, "WT", 15, method = "linear")$value))
})

test_that("corrections supersede evidence rather than mutating it", {
  patient <- lator_patient_new("P001")
  patient <- lator_patient_add_event(patient, "covariate", 0, "WT", 60, "kg")
  earlier <- patient$events[[1L]]$event_id
  patient <- lator_patient_add_event(patient, "covariate", 0, "WT", 65, "kg", supersedes = earlier)
  expect_equal(length(patient$events), 2L)
  expect_equal(lator_covariate_at(patient, "WT", 0)$value, 65)
})

test_that("declared treatment-interaction covariates use active medication profiles", {
  patient <- lator_test_patient()
  patient <- lator_patient_medication_add(
    patient, "warfarin", "vitamin-k-antagonist"
  )
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT", "CMT", "DV", "MDV",
              "COMED_WARFARIN"),
    ADVAN = 1,
    PRED = paste0(
      "CL=THETA(1)*(1+0.2*COMED_WARFARIN)*exp(ETA(1));",
      "V=THETA(2);S1=V"
    ),
    ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(THETA = 1:2, Value = c(3, 30)),
    OMEGAS = data.frame(OMEGA = 1, Value = 0.2),
    SIGMAS = data.frame(SIGMA = 1, Value = 0.4),
    COVARIATES = "COMED_WARFARIN"
  )
  prepared <- .lator_patient_dataset(patient, model, "Drug A")
  expect_true(all(prepared$data$COMED_WARFARIN == 1))
  expect_equal(
    prepared$evidence$COMED_WARFARIN$source,
    "patient-treatment-profile"
  )
})

test_that("effective-dated interaction evidence overrides treatment fallback", {
  patient <- lator_test_patient()
  patient <- lator_patient_medication_add(
    patient, "warfarin", "vitamin-k-antagonist"
  )
  patient <- lator_patient_add_event(
    patient, "covariate", 0, "COMED_WARFARIN", 0
  )
  patient <- lator_patient_add_event(
    patient, "covariate", 10, "COMED_WARFARIN", 1
  )
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT", "CMT", "DV", "MDV",
              "COMED_WARFARIN"),
    ADVAN = 1,
    PRED = paste0(
      "CL=THETA(1)*(1+0.2*COMED_WARFARIN)*exp(ETA(1));",
      "V=THETA(2);S1=V"
    ),
    ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(THETA = 1:2, Value = c(3, 30)),
    OMEGAS = data.frame(OMEGA = 1, Value = 0.2),
    SIGMAS = data.frame(SIGMA = 1, Value = 0.4),
    COVARIATES = "COMED_WARFARIN"
  )
  prepared <- .lator_patient_dataset(patient, model, "Drug A")
  expect_true(all(
    prepared$data$COMED_WARFARIN[prepared$data$TIME < 10] == 0
  ))
  expect_true(all(
    prepared$data$COMED_WARFARIN[prepared$data$TIME > 10] == 1
  ))
  expect_setequal(
    prepared$data$COMED_WARFARIN[prepared$data$TIME == 10],
    c(0, 1)
  )
  expect_false(identical(
    prepared$evidence$COMED_WARFARIN$source,
    "patient-treatment-profile"
  ))
})
