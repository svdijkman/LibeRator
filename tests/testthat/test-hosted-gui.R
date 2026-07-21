test_that("hosted dosing GUI uses a separate workspace path per session", {
  root <- tempfile("lator-hosted-")
  app <- lator_gui(
    path = root, session_workspace = TRUE, launch.browser = NULL
  )
  expect_s3_class(app, "shiny.appobj")
  server <- app[["serverFuncSource"]]()
  paths <- character()
  shiny::testServer(server, {
    paths <<- c(paths, session_path)
  })
  shiny::testServer(server, {
    paths <<- c(paths, session_path)
  })
  expect_length(unique(paths), 2L)
  expect_true(all(startsWith(
    normalizePath(paths, winslash = "/", mustWork = FALSE),
    paste0(normalizePath(root, winslash = "/", mustWork = FALSE), "/sessions/")
  )))
})

test_that("empty-workspace GUI refreshes after every successive evidence write", {
  root <- tempfile("lator-empty-gui-")
  app <- lator_gui(
    path = root, passphrase = "empty workspace test passphrase",
    launch.browser = NULL
  )
  server <- app[["serverFuncSource"]]()

  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "new_patient", patient_id = "P-EMPTY-001",
      study_id = "TEST", label = "Empty workspace test", nonce = 1
    ))
    session$flushReact()
    expect_equal(workbench_payload()$patient$id, "P-EMPTY-001")
    expect_equal(workbench_payload()$patient$revision, 1L)

    evidence <- list(
      list(type = "dose", time = "0", name = "Example AED", value = "300", unit = "mg"),
      list(type = "covariate", time = "1", name = "WT", value = "68", unit = "kg"),
      list(type = "dose", time = "12", name = "Example AED", value = "300", unit = "mg"),
      list(type = "concentration", time = "14", name = "Example AED", value = "5.2", unit = "mg/L")
    )
    for (index in seq_along(evidence)) {
      event <- c(list(action = "add_event", nonce = index + 1L), evidence[[index]])
      session$setInputs(liberator_workbench_event = event)
      session$flushReact()
      expect_length(workbench_payload()$events, index)
      expect_equal(workbench_payload()$patient$revision, index + 1L)
      expect_equal(state$data_revision, index + 1L)
    }

    stored <- lator_patient_get(state$workspace, "P-EMPTY-001")
    expect_equal(vapply(stored$events, `[[`, character(1), "type"),
                 c("dose", "covariate", "dose", "concentration"))
  })
})

test_that("hosted teaching sessions seed a usable synthetic case", {
  root <- tempfile("lator-hosted-teaching-")
  app <- lator_gui(
    path = root, session_workspace = TRUE, teaching_example = TRUE,
    launch.browser = NULL
  )
  server <- app[["serverFuncSource"]]()

  shiny::testServer(server, {
    session$setInputs(
      lator_passphrase = "hosted teaching test passphrase",
      lator_unlock = 1
    )
    session$flushReact()
    expect_equal(state$patient_id, "TEACH-AED-001")
    expect_true("teaching-aed" %in% names(state$models))
    expect_true("aed-example-aed" %in% names(state$endpoints))
    expect_length(workbench_payload()$events, 9L)
    expect_match(state$status$text, "synthetic teaching patient", ignore.case = TRUE)
  })
})
