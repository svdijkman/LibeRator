test_that("therapeutic GUI retains shared theme, dove, and responsive controls", {
  script <- paste(readLines(
    system.file("htmlwidgets", "liberatorWorkbench.js", package = "LibeRator"),
    warn = FALSE
  ), collapse = "\n")
  base_css <- paste(readLines(
    system.file("htmlwidgets", "liberatorWorkbench.css", package = "LibeRator"),
    warn = FALSE
  ), collapse = "\n")
  extras_css <- paste(readLines(
    system.file("htmlwidgets", "liberatorExtras.css", package = "LibeRator"),
    warn = FALSE
  ), collapse = "\n")
  design <- paste(readLines(
    system.file("htmlwidgets", "liber-design-system.js", package = "LibeRator"),
    warn = FALSE
  ), collapse = "\n")

  expect_match(design, 'localStorage\\.getItem\\("liber\\.theme"\\)')
  expect_match(design, "liber-task-state", fixed = TRUE)
  expect_match(script, "LibeRDesign.theme", fixed = TRUE)
  expect_match(script, "LibeRDesign.taskState", fixed = TRUE)
  expect_match(script, "cancel_task", fixed = TRUE)
  expect_match(script, "lr-endpoint-template-row", fixed = TRUE)
  expect_match(script, "create_endpoint", fixed = TRUE)
  expect_match(script, "select_drug", fixed = TRUE)
  expect_match(script, "endpointPrompt", fixed = TRUE)
  expect_match(script, "Drug preset", fixed = TRUE)
  expect_match(script, "Modify endpoint", fixed = TRUE)
  expect_match(script, "revise_endpoint", fixed = TRUE)
  expect_match(script, "EndpointSetModal", fixed = TRUE)
  expect_match(script, "Combine endpoints", fixed = TRUE)
  expect_match(script, "create_endpoint_set", fixed = TRUE)
  expect_match(script, "revise_endpoint_set", fixed = TRUE)
  expect_match(script, "regimenDefaults", fixed = TRUE)
  expect_match(script, "defaults.amounts", fixed = TRUE)
  expect_match(script, "Joint attainment", fixed = TRUE)
  expect_match(script, "Expected utility", fixed = TRUE)
  expect_match(script, "Hard constraints", fixed = TRUE)
  expect_match(script, "Pareto", fixed = TRUE)
  expect_match(script, "MedicationModal", fixed = TRUE)
  expect_match(script, "add_medication", fixed = TRUE)
  expect_match(script, "Next: choose endpoint", fixed = TRUE)
  expect_match(script, "role:\"radio\"", fixed = TRUE)
  expect_match(script, "DeletePatientModal", fixed = TRUE)
  expect_match(script, 'Type "YES" to confirm', fixed = TRUE)
  expect_match(script, "monitoring_analytes", fixed = TRUE)
  expect_match(script, "openMedication", fixed = TRUE)
  expect_match(script, "IndividualProfile", fixed = TRUE)
  expect_match(script, "individualParameters", fixed = TRUE)
  expect_match(script, "selectedCandidates", fixed = TRUE)
  expect_match(script, "lr-stacked-forecasts", fixed = TRUE)
  expect_match(script, "Assessment readiness", fixed = TRUE)
  expect_match(script, "Medication, endpoint & model", fixed = TRUE)
  expect_match(script, "lr-header-version", fixed = TRUE)
  expect_match(script, "selectTab:tab[1]", fixed = TRUE)
  expect_match(script, "individualised===true", fixed = TRUE)
  expect_match(script, "Changed since the most recent individualisation",
               fixed = TRUE)
  expect_match(script, "Estimate stable patient parameters", fixed = TRUE)
  expect_match(script, "Estimate parameters changing over time", fixed = TRUE)
  expect_match(script, "modelDiscoveryAvailable", fixed = TRUE)
  expect_match(script, "Add/select model", fixed = TRUE)
  expect_match(script, "ModelLibraryModal", fixed = TRUE)
  expect_match(script, "load_model_library", fixed = TRUE)
  expect_match(script, "model_import_library", fixed = TRUE)
  expect_match(script, "model_create_template", fixed = TRUE)
  expect_match(script, "Create from LibeRation template", fixed = TRUE)
  expect_match(script, "ModelInfoModal", fixed = TRUE)
  expect_match(script, "Describe selected model", fixed = TRUE)
  expect_match(script, "Dosing interval (hours, optional)", fixed = TRUE)
  expect_match(script, "Steady state before this dose", fixed = TRUE)
  expect_match(script, "steady_state", fixed = TRUE)
  expect_match(script, "Individualised post-dose PK profile", fixed = TRUE)
  expect_match(script, "AssessmentModal", fixed = TRUE)
  expect_match(
    script, "Current monitoring episode (recommended)", fixed = TRUE
  )
  expect_match(
    script, "all eligible evidence was retained in the fit", fixed = TRUE
  )
  expect_match(script, "dynamicReady", fixed = TRUE)
  expect_match(script, "TaskProgress", fixed = TRUE)
  expect_match(script, "Exploring candidate regimens", fixed = TRUE)
  expect_match(script, "Generating future predictions", fixed = TRUE)
  expect_match(script, "Individualised steady-state exposure", fixed = TRUE)
  expect_match(script, "Peak-trough fluctuation", fixed = TRUE)
  expect_match(
    script,
    "95% similar-patient prediction interval (ETA variability)",
    fixed = TRUE
  )
  expect_match(script, "populationLower", fixed = TRUE)
  expect_match(script, "legacy_event_only", fixed = TRUE)
  expect_match(script, "older worker", fixed = TRUE)
  expect_match(script, "SS target attainment", fixed = TRUE)
  expect_match(script, "Periodic steady-state dosing interval", fixed = TRUE)
  expect_match(script, "Transition near steady state", fixed = TRUE)
  expect_match(script, "Derived model quantities", fixed = TRUE)
  expect_match(extras_css, "\\.lr-model-library-card")
  expect_match(extras_css, "\\.lr-model-source-tabs")
  expect_match(extras_css, "\\.lr-model-info-trigger")
  expect_match(
    extras_css,
    "width:\\s*15px;[\\s\\S]*height:\\s*15px;",
    perl = TRUE
  )
  expect_match(script, "MedicationAddButton", fixed = TRUE)
  expect_match(extras_css, "\\.lr-add-medication-mark::before")
  expect_match(
    script,
    "similar-patient prediction interval (ETA variability)",
    fixed = TRUE
  )
  expect_match(script, "EndpointOutcomeTable", fixed = TRUE)
  expect_match(script, "Numerical conditional-draw outcomes", fixed = TRUE)
  expect_match(script, "prediction.endpointOutcomes", fixed = TRUE)
  expect_match(script, "Endpoint intervals are quantiles", fixed = TRUE)
  expect_match(extras_css, ".lr-endpoint-outcome-table", fixed = TRUE)
  expect_match(
    script, "these are not confidence intervals for a population mean", fixed = TRUE
  )
  expect_match(script, "Burden-adjusted score", fixed = TRUE)
  expect_false(grepl('"R constructor"', script, fixed = TRUE))
  favicon <- paste(readLines(
    system.file("assets", "favicon.svg", package = "LibeRator"),
    warn = FALSE
  ), collapse = "")
  expect_match(favicon, 'id="liberator-dove"', fixed = TRUE)
  expect_match(favicon, "data:image/png;base64,", fixed = TRUE)
  expect_match(script, "lr-sidebar-toggle", fixed = TRUE)
  expect_match(script, "lr-rail-toggle", fixed = TRUE)
  expect_match(script, "useDialogFocus", fixed = TRUE)
  expect_match(base_css, "--brand:", fixed = TRUE)
  expect_false(grepl("--purple", paste(base_css, extras_css)))
  expect_match(
    base_css,
    "grid-template-rows:58px 32px minmax(0,1fr) 27px",
    fixed = TRUE
  )
  expect_match(base_css, ".lr-logo{width:42px;height:42px", fixed = TRUE)
  expect_match(base_css, ".lr-button{min-height:32px", fixed = TRUE)
  expect_match(base_css, ".lr-panel{margin-bottom:10px;border:1px solid var(--line);border-radius:10px", fixed = TRUE)
  expect_match(extras_css, "\\.lr-sidebar\\.open")
  expect_match(extras_css, "\\.lr-right\\.open")
  expect_match(extras_css, "\\.lr-danger")
  expect_match(extras_css, "\\.lr-check-row small\\s*\\{\\s*display:\\s*block")
  expect_match(extras_css, "\\.lr-profile-metrics")
  expect_match(extras_css, "\\.lr-population-limit")
  expect_match(extras_css, "\\.lr-forecast-limit")
  expect_match(script, "Dashed limits:", fixed = TRUE)
  expect_false(grepl("lr-population-band", script, fixed = TRUE))
  expect_false(grepl("lr-forecast-band", script, fixed = TRUE))
  expect_match(extras_css, "\\.lr-task-progress")
  expect_match(extras_css, "\\.lr-steady-cycle")
  expect_match(extras_css, "\\.lr-header-version")
  expect_match(extras_css, "\\.lr-readiness \\.changed span")
  expect_match(extras_css, "\\.lr-parameter-footnote")
  expect_match(extras_css, "\\.lr-endpoint-set-table")
  expect_match(extras_css, "\\.lr-objective-components")
  expect_match(extras_css, "\\.lr-component-attainment")
  expect_match(script, "EvidenceCorrectionModal", fixed = TRUE)
  expect_match(script, '"correct_event"', fixed = TRUE)
  expect_match(script, "Mark as entered in error", fixed = TRUE)
  expect_match(extras_css, "\\.lr-evidence-ledger")
  expect_match(extras_css, "\\.lr-correction-original")
})
