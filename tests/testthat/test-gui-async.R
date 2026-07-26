test_that("future prediction background entry point preserves selection", {
  patient <- lator_test_patient()
  endpoint <- lator_endpoint_aed(
    "Drug A", 1, 4, "mg/L", "teaching source"
  )
  assessment <- lator_assess(patient, lator_test_model(), endpoint)
  candidates <- lator_regimen_candidates(
    amounts = 50, intervals = 12, horizon = 24
  )
  compared <- lator_regimen_optimise(
    assessment, patient, candidates, nsim = 2, grid_step = 4, seed = 1
  )
  selected <- compared$summary$candidate_id[[1L]]
  prediction <- .lator_gui_background_task(
    "predict",
    list(regimen = compared, candidate_id = selected)
  )
  expect_s3_class(prediction, "lator_future_prediction")
  expect_identical(prediction$candidate_id, selected)
})
