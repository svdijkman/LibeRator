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
})
