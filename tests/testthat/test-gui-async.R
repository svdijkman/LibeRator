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
  expect_named(prediction, selected)
  expect_s3_class(prediction[[selected]], "lator_future_prediction")
  expect_identical(prediction[[selected]]$candidate_id, selected)

  selected_many <- compared$summary$candidate_id[seq_len(min(
    2L, nrow(compared$summary)
  ))]
  predictions <- .lator_gui_background_task(
    "predict",
    list(regimen = compared, candidate_id = selected_many)
  )
  expect_named(predictions, selected_many)
  expect_true(all(vapply(
    predictions, inherits, logical(1), "lator_future_prediction"
  )))
})
