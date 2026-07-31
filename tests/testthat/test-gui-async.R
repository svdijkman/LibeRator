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

test_that("assessment background entry point retains requested TDM display", {
  assessment <- .lator_gui_background_task(
    "assess",
    list(
      patient = lator_test_patient(),
      model = lator_test_model(),
      endpoint = lator_endpoint_aed(
        "Drug A", 1, 4, "mg/L", "teaching source"
      ),
      mode = "static",
      process_scale = 0.1,
      profile_observation_scope = "all",
      profile_observation_count = 2L,
      profile_observation_since = NA_real_
    )
  )
  observations <- assessment$individual_profile[
    assessment$individual_profile$kind == "observation", , drop = FALSE
  ]
  expect_equal(nrow(observations), 2L)
  expect_equal(observations$OBSERVED_TIME, c(2, 14))
  expect_identical(
    assessment$individual_profile_interval$observation_selection$scope,
    "all"
  )
})

test_that("callr GUI workers use the active LibeRator implementation", {
  skip_if_not_installed("callr")
  patient <- lator_test_patient()
  endpoint <- lator_endpoint_aed(
    "Drug A", 1, 4, "mg/L", "teaching source"
  )
  assessment <- lator_assess(
    patient, lator_test_model(), endpoint,
    profile_population_draws = 8L
  )
  compared <- lator_regimen_optimise(
    assessment, patient,
    lator_regimen_candidates(50, 12, horizon = 24),
    nsim = 2, grid_step = 4, seed = 1
  )
  selected <- compared$summary$candidate_id[[1L]]
  registry <- .liber_shared_task_registry()
  on.exit(.liber_shared_task_cancel_all(registry), add = TRUE)
  .liber_shared_task_start(
    registry, "LibeRator", ".lator_gui_background_task",
    args = list(
      operation = "predict",
      arguments = list(regimen = compared, candidate_id = selected)
    )
  )
  for (attempt in seq_len(200L)) {
    Sys.sleep(0.05)
    .liber_shared_task_poll(registry)
    if (!.liber_shared_task_active(registry)) break
  }
  expect_false(.liber_shared_task_active(registry))
  completed <- .liber_shared_task_take_completed(registry)
  expect_length(completed, 1L)
  expect_identical(completed[[1L]]$status, "completed")
  expect_s3_class(completed[[1L]]$result[[selected]], "lator_future_prediction")
})
