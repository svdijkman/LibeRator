test_that("static and dynamic assessments use the C++ individual objective", {
  endpoint <- lator_endpoint_aed("Drug A", 1, 4, "mg/L", "teaching source")
  static <- lator_assess(lator_test_patient(), lator_test_model(), endpoint)
  expect_s3_class(static, "lator_assessment")
  expect_equal(static$convergence, 0L)
  expect_equal(nrow(static$eta_trajectory), 1L)
  expect_true(all(c("CL", "V") %in% static$individual_parameters$parameter))
  cl_row <- static$individual_parameters$parameter == "CL"
  v_row <- static$individual_parameters$parameter == "V"
  expect_true(all(static$individual_parameters$individualised[cl_row]))
  expect_false(any(static$individual_parameters$individualised[v_row]))
  expect_true(all(c("TIME", "IPRED", "DV") %in% names(static$predictions)))
  expect_true(is.finite(static$diagnostics$gradient_max))
  curve <- static$individual_profile[
    static$individual_profile$kind == "curve", , drop = FALSE
  ]
  observed <- static$individual_profile[
    static$individual_profile$kind == "observation", , drop = FALSE
  ]
  expect_equal(range(curve$TIME), c(12, 24))
  expect_equal(unique(round(diff(curve$TIME), 8)), 0.25)
  expect_equal(observed$TIME, c(14, 14))
  expect_equal(observed$OBSERVED_TIME, c(2, 14))
  expect_equal(
    static$individual_profile_interval$observation_selection$selected_count,
    2L
  )
  expect_equal(static$individual_profile_interval$hours, 12)
  expect_equal(
    static$individual_profile_interval$source,
    "recent dose-time spacing"
  )
  expect_identical(
    static$individual_profile_interval$profile_type, "time_course"
  )
  expect_true(all(vapply(
    static$individual_profile_interval$summary[
      c("mean_css", "trough", "peak", "fluctuation_percent")
    ],
    function(value) length(value) == 1L && is.finite(value),
    logical(1)
  )))

  dynamic <- lator_assess(lator_test_patient(boundary = TRUE), lator_test_model(), endpoint,
                          mode = "dynamic", process_scale = 0.2)
  expect_equal(length(dynamic$eta), 2L)
  expect_equal(unique(dynamic$eta_trajectory$occasion), 1:2)
  expect_true(any(dynamic$data$.LATOR_ROLE == "prechange"))
  expect_true(any(dynamic$data$.LATOR_ROLE == "postchange"))
})

test_that("readiness tracks which inputs changed after individualisation", {
  workspace <- lator_workspace(
    tempfile("lator-readiness-change-"),
    "readiness change test passphrase"
  )
  patient <- lator_test_patient()
  endpoint <- lator_endpoint_aed(
    "Drug A", 1, 4, "mg/L", "teaching source"
  )
  endpoint_key <- .lator_patient_endpoint_instance_key(
    patient, "Drug A", endpoint
  )
  patient <- lator_patient_endpoint_set(
    patient, "Drug A", endpoint_key, endpoint
  )
  assessment <- lator_assess(patient, lator_test_model(), endpoint)
  patient$assessments <- list(assessment)
  patient <- lator_patient_save(workspace, patient)

  payload <- .lator_gui_payload(
    workspace, patient$patient_id,
    models = list(teaching = lator_test_model()),
    endpoints = stats::setNames(list(endpoint), endpoint_key),
    selected_model = "teaching",
    selected_endpoint = endpoint_key,
    selected_drug = "drug-a"
  )
  expect_false(payload$readiness$anyChanged)

  patient <- lator_patient_add_event(
    patient, "concentration", 20, "Drug A", 2.5, "mg/L"
  )
  patient <- lator_patient_save(
    workspace, patient, expected_revision = patient$revision
  )
  changed <- .lator_gui_payload(
    workspace, patient$patient_id,
    models = list(teaching = lator_test_model()),
    endpoints = stats::setNames(list(endpoint), endpoint_key),
    selected_model = "teaching",
    selected_endpoint = endpoint_key,
    selected_drug = "drug-a"
  )
  expect_true(changed$readiness$changed$tdm)
  expect_false(changed$readiness$changed$medication)
  expect_false(changed$readiness$changed$patient)
  expect_true(changed$readiness$anyChanged)
})

test_that("time-changing assessment requires TDM evidence in two states", {
  patient <- lator_test_patient()
  status <- .lator_dynamic_evidence_status(patient, "Drug A")
  expect_false(status$ready)
  expect_equal(status$observed_state_count, 1L)
  expect_match(status$reason, "needs at least two patient states", fixed = TRUE)
  expect_error(
    lator_assess(
      patient, lator_test_model(),
      lator_endpoint_aed("Drug A", 1, 4, "mg/L", "teaching source"),
      mode = "dynamic"
    ),
    "needs at least two patient states"
  )

  ready <- .lator_dynamic_evidence_status(
    lator_test_patient(boundary = TRUE), "Drug A"
  )
  expect_true(ready$ready)
  expect_equal(ready$observed_state_count, 2L)
})

test_that("monitoring episodes retain recent TDM without showing remote history", {
  patient <- lator_patient_new("MONITORING-EPISODES")
  add <- function(record, type, time, value, metadata = list()) {
    lator_patient_add_event(
      record, type, time, "Drug A", value,
      if (type == "dose") "mg" else "mg/L",
      metadata = metadata
    )
  }
  patient <- add(
    patient, "dose", 0, 100,
    list(ii = 12, ss = 1L, route = "oral", cmt = 1L)
  )
  patient <- add(patient, "concentration", 12, 4)
  patient <- add(
    patient, "dose", 8760, 150,
    list(ii = 12, ss = 1L, route = "oral", cmt = 1L)
  )
  patient <- add(patient, "concentration", 8772, 6)
  patient <- add(
    patient, "dose", 9096, 150,
    list(ii = 12, ss = 1L, route = "oral", cmt = 1L)
  )
  patient <- add(patient, "concentration", 9108, 7)

  automatic <- .lator_profile_observation_selection(
    patient, "Drug A", scope = "automatic"
  )
  expect_equal(
    vapply(automatic$events, `[[`, numeric(1), "time"),
    c(8772, 9108)
  )
  expect_equal(automatic$total_count, 3L)
  expect_equal(automatic$selected_count, 2L)

  latest <- .lator_profile_observation_selection(
    patient, "Drug A", scope = "latest"
  )
  expect_equal(
    vapply(latest$events, `[[`, numeric(1), "time"), 9108
  )
  all_history <- .lator_profile_observation_selection(
    patient, "Drug A", scope = "all"
  )
  expect_equal(all_history$selected_count, 3L)

  window <- .lator_postdose_window(patient, "Drug A")
  rows <- .lator_profile_observation_rows(
    patient, "Drug A", Inf, window, scope = "automatic"
  )
  expect_equal(rows$data$OBSERVED_TIME, c(8772, 9108))
  expect_equal(rows$data$TIME, c(9108, 9108))
})

test_that("recorded dosing interval takes precedence for dense profiles", {
  patient <- lator_test_patient()
  patient$events[[3L]]$metadata$ii <- 8
  window <- .lator_postdose_window(patient, "Drug A")
  expect_equal(window$start, 12)
  expect_equal(window$interval, 8)
  expect_equal(window$source, "recorded dosing interval")
  expect_false(window$assumed)
})

test_that("dosing intervals without steady state are not silently accumulated", {
  patient <- lator_test_patient()
  patient$events[[1L]]$metadata$ii <- 12
  assessment <- lator_assess(
    patient, lator_test_model(),
    lator_endpoint_aed("Drug A", 1, 4, "mg/L", "teaching source")
  )
  expect_true(any(grepl(
    "without steady-state dosing", assessment$warnings, fixed = TRUE
  )))
  patient$events[[1L]]$metadata$ss <- 1L
  assessment <- lator_assess(
    patient, lator_test_model(),
    lator_endpoint_aed("Drug A", 1, 4, "mg/L", "teaching source")
  )
  expect_false(any(grepl(
    "without steady-state dosing", assessment$warnings, fixed = TRUE
  )))
})

test_that("the superseded He volume translation is rejected explicitly", {
  model <- lator_test_model(TRUE)
  model$PRED <- paste(
    model$PRED, "V=THETA(6)*WT", sep = ";"
  )
  attr(model, "library_provenance") <- list(
    source = "LibeRary",
    library_id = "aedapt_lamotrigine_he",
    library_version = "1.0.0"
  )
  expect_error(
    lator_assess(
      lator_test_patient(covariate = TRUE), model,
      lator_endpoint_aed("Drug A", 1, 4, "mg/L", "teaching source")
    ),
    "superseded LibeRary 1.0.0 translation"
  )
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
  expect_true(all(c(
    "steady_state_mean", "steady_state_trough", "steady_state_peak",
    "whole_cycle_in_range_probability",
    "horizon_steady_state_probability"
  ) %in% names(compared$summary)))
  expect_true(all(is.finite(
    compared$summary$steady_state_mean[compared$summary$feasible]
  )))

  selected <- compared$summary$candidate_id[[1L]]
  forecast <- lator_regimen_predict(compared, selected)
  expect_s3_class(forecast, "lator_future_prediction")
  expect_identical(forecast$candidate_id, selected)
  expect_identical(forecast$assessment_id, assessment$assessment_id)
  expect_true(nrow(forecast$forecast) > 1L)
  expect_true(all(c("time", "lower", "median", "upper", "draws") %in% names(forecast$forecast)))
  expect_true(all(forecast$forecast$lower <= forecast$forecast$median))
  expect_true(all(forecast$forecast$median <= forecast$forecast$upper))
  expect_identical(
    forecast$uncertainty$interval_type,
    "pointwise_posterior_prediction"
  )
  expect_identical(
    forecast$uncertainty$sources,
    "individual_eta_posterior"
  )
  expect_false(forecast$uncertainty$residual_measurement_variability)
  expect_false(forecast$uncertainty$population_parameter_uncertainty)
  expect_equal(forecast$uncertainty$probabilities, c(0.05, 0.5, 0.95))
  expect_gt(nrow(forecast$steady_state_forecast), 1L)
  expect_identical(forecast$steady_state$profile_type, "time_course")
  expect_true(is.finite(
    forecast$steady_state$summary$mean_css$median
  ))
  expect_true(is.character(
    forecast$steady_state$horizon_convergence$status
  ))
})

test_that("regimen comparison applies multi-endpoint decision rules", {
  patient <- lator_test_patient()
  primary <- lator_endpoint_aed(
    "Drug A", 1, 4, "mg/L", "Clinical objective protocol"
  )
  safety <- lator_endpoint(
    "drug-a-safety-range", "Drug A safety range", "Drug A",
    "therapeutic_range", "last_interval_average", "mg/L",
    rules = list(lower = 0.1, upper = 8, target = 3),
    source = "Clinical objective protocol"
  )
  objective <- lator_endpoint_set(
    "drug-a-benefit-risk", "Drug A benefit-risk objective", "Drug A",
    list(
      lator_endpoint_component(primary, "primary", weight = 2),
      lator_endpoint_component(
        safety, "safety", weight = 1,
        hard_constraint = TRUE, minimum_attainment = 0
      )
    ),
    source = "Clinical objective protocol"
  )
  assessment <- lator_assess(
    patient, lator_test_model(), objective,
    profile_population_draws = 0
  )
  candidates <- lator_regimen_candidates(
    c(50, 100), c(12, 24), horizon = 24
  )
  compared <- lator_regimen_optimise(
    assessment, patient, candidates, endpoint = objective,
    nsim = 4, grid_step = 2, seed = 17
  )
  expect_identical(compared$version, 2L)
  expect_true(all(c(
    "joint_attainment_probability", "primary_attainment_probability",
    "expected_utility", "hard_constraints_pass", "decision_eligible",
    "pareto_optimal", "rank"
  ) %in% names(compared$summary)))
  expect_true(all(compared$summary$hard_constraints_pass))
  expect_true(any(compared$summary$pareto_optimal))
  expect_equal(compared$summary$rank, seq_len(nrow(compared$summary)))
  expect_true(all(
    compared$summary$expected_utility >= 0 &
      compared$summary$expected_utility <= 1
  ))
  selected <- compared$summary$candidate_id[[1L]]
  forecast <- lator_regimen_predict(compared, selected)
  expect_identical(forecast$version, 2L)
  expect_equal(nrow(forecast$evaluation$components), 2L)
  expect_equal(nrow(forecast$endpoint_outcomes), 2L)
  expect_true(all(c(
    "name", "role", "metric", "display_unit", "lower", "median", "upper",
    "target", "attainment_probability", "hard_constraint",
    "minimum_attainment", "constraint_pass"
  ) %in% names(forecast$endpoint_outcomes)))
  expect_true(all(is.finite(forecast$endpoint_outcomes$median)))
  expect_true(all(nzchar(forecast$endpoint_outcomes$target)))
  expect_true(is.finite(
    forecast$evaluation$joint_attainment_probability
  ))
})

test_that("candidate dose covariates follow the proposed regimen", {
  model <- lator_test_model(TRUE)
  model$COVARIATES <- c(
    model$COVARIATES, "DAILY_DOSE", "DOSE_MG_KG_DAY", "CBZ_DAILY_DOSE"
  )
  data <- data.frame(
    WT = c(50, 100), DAILY_DOSE = c(200, 200),
    DOSE_MG_KG_DAY = c(4, 2), CBZ_DAILY_DOSE = c(400, 400),
    .LATOR_ROLE = c("observation", "future")
  )
  candidate <- data.frame(amount = 100, interval = 24)
  updated <- .lator_candidate_covariates(data, candidate, model)
  expect_equal(updated$DAILY_DOSE, c(200, 100))
  expect_equal(updated$DOSE_MG_KG_DAY, c(4, 1))
  expect_equal(updated$CBZ_DAILY_DOSE, c(400, 400))
})

test_that("direct models are only labelled mean-only when time invariant", {
  model <- lator_test_model()
  model$SOLVER <- "direct"
  model$PRED_MODE <- "pred"
  model$PRED <- "CL=THETA(1)*exp(ETA(1));F=DAILY_DOSE/CL"
  expect_identical(
    .lator_model_profile_type(model), "steady_state_mean_only"
  )
  model$PRED <- paste(model$PRED, "F=F*exp(-TIME/24)", sep = ";")
  expect_identical(.lator_model_profile_type(model), "time_course")
})

test_that("Rivas steady-state individualisation agrees with AEDapt reference", {
  model <- LibeRation::nm_model(
    INPUT = c(
      "ID", "TIME", "EVID", "AMT", "RATE", "II", "SS", "CMT",
      "DV", "MDV", "WT", "COMED_VPA", "COMED_PHT", "COMED_PHB",
      "COMED_CBZ"
    ),
    ADVAN = 2, TRANS = 2, DOSECMP = 1, OBSCMP = 2,
    PRED = paste(
      "NIND=COMED_PHT+COMED_PHB+COMED_CBZ",
      "IND=ifelse(NIND>1,1,0)",
      paste0(
        "CL=THETA(1)*WT*exp(THETA(2)*COMED_VPA+",
        "THETA(3)*COMED_PHT+THETA(4)*COMED_PHB+",
        "THETA(5)*COMED_CBZ+THETA(6)*IND+ETA(1))"
      ),
      "V=THETA(7)*WT", "KA=THETA(8)", "S2=V", sep = ";"
    ),
    ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(
      THETA = 1:8,
      Value = c(0.028, -0.713, 0.663, 0.588, 0.467, 0.864, 1.5, 1.3)
    ),
    OMEGAS = data.frame(OMEGA = 1, Value = 0.07285),
    SIGMAS = data.frame(SIGMA = 1, Value = 1.25),
    COVARIATES = c(
      "WT", "COMED_VPA", "COMED_PHT", "COMED_PHB", "COMED_CBZ"
    )
  )
  patient <- lator_patient_new("RIVAS-AEDAPT-REFERENCE")
  patient <- lator_patient_add_event(
    patient, "covariate", 0, "WT", 90, "kg"
  )
  patient <- lator_patient_add_event(
    patient, "dose", 0, "lamotrigine", 100, "mg",
    metadata = list(ii = 12, ss = 1L)
  )
  patient <- lator_patient_add_event(
    patient, "concentration", 12, "lamotrigine", 12, "mg/L"
  )
  assessment <- lator_assess(
    patient, model,
    lator_endpoint_aed(
      "lamotrigine", 4, 12, "mg/L", "AEDapt parity scenario"
    ),
    maxit = 100
  )
  expect_equal(assessment$eta[[1L]], -1.1010, tolerance = 5e-4)
  expect_equal(
    assessment$individual_profile_interval$summary$mean_css,
    9.944, tolerance = 0.02
  )
  curve <- assessment$individual_profile[
    assessment$individual_profile$kind == "curve", , drop = FALSE
  ]
  expect_equal(range(curve$IPRED), c(9.6246, 10.1857), tolerance = 2e-3)
  expect_equal(
    range(curve$POP_LOWER), c(1.6723, 2.2417), tolerance = 0.02
  )
  expect_equal(
    range(curve$POP_UPPER), c(5.4141, 5.9819), tolerance = 0.02
  )
  expect_identical(
    assessment$individual_profile_interval$population_interval$variability,
    "between-subject ETA variability only"
  )
})
