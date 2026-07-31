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

test_that("pre-unlocked workspace hydrates outside a reactive consumer", {
  workspace <- lator_workspace(
    tempfile("lator-startup-"),
    "pre-unlocked startup test passphrase"
  )
  lator_patient_save(
    workspace,
    lator_patient_new("P-STARTUP-001")
  )
  app <- lator_gui(workspace = workspace, launch.browser = NULL)
  server <- app[["serverFuncSource"]]()
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)

  expect_no_error(server(session$input, session$output, session))
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
    session$setInputs(liberator_workbench_event = list(
      action = "add_medication", drug = "Example AED",
      therapeutic_class = "antiseizure",
      configure_endpoint = FALSE, nonce = 2
    ))
    session$flushReact()

    evidence <- list(
      list(type = "dose", time = "0", name = "Example AED", treatment_drug = "Example AED", value = "300", unit = "mg"),
      list(type = "covariate", time = "1", name = "WT", value = "68", unit = "kg"),
      list(type = "dose", time = "12", name = "Example AED", treatment_drug = "Example AED", value = "300", unit = "mg"),
      list(type = "concentration", time = "14", name = "Example AED", treatment_drug = "Example AED", value = "5.2", unit = "mg/L")
    )
    for (index in seq_along(evidence)) {
      event <- c(list(action = "add_event", nonce = index + 2L), evidence[[index]])
      session$setInputs(liberator_workbench_event = event)
      session$flushReact()
      expect_length(workbench_payload()$events, index)
      expect_equal(workbench_payload()$patient$revision, index + 2L)
      expect_equal(state$data_revision, index + 2L)
    }

    stored <- lator_patient_get(state$workspace, "P-EMPTY-001")
    expect_equal(vapply(stored$events, `[[`, character(1), "type"),
                 c("dose", "covariate", "dose", "concentration"))
  })
})

test_that("GUI appends clinician evidence corrections and exposes full lineage", {
  root <- tempfile("lator-correction-gui-")
  app <- lator_gui(
    path = root, passphrase = "correction gui test passphrase",
    launch.browser = NULL
  )
  server <- app[["serverFuncSource"]]()

  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "new_patient", patient_id = "P-CORRECTION-001",
      study_id = "TEST", label = "Correction test", nonce = 1
    ))
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "add_medication", drug = "Drug A",
      therapeutic_class = "test", configure_endpoint = FALSE, nonce = 2
    ))
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "add_event", type = "dose", time = "0",
      name = "Drug A", treatment_drug = "Drug A",
      value = "100", unit = "mg", route = "oral", rate = "0",
      dosing_interval = "12", steady_state = FALSE, nonce = 3
    ))
    session$flushReact()
    original_id <- workbench_payload()$events[[1L]]$id

    session$setInputs(liberator_workbench_event = list(
      action = "correct_event", event_id = original_id,
      reason = "Dose transcribed incorrectly", actor = "clinician-17",
      entered_in_error = FALSE, type = "dose", time = "0",
      name = "Drug A", treatment_drug = "Drug A",
      value = "150", unit = "mg", source = "clinic form",
      route = "oral", rate = "0", dosing_interval = "12",
      steady_state = FALSE, nonce = 4
    ))
    session$flushReact()

    payload <- workbench_payload()
    expect_length(payload$eventLedger, 2L)
    expect_length(payload$events, 1L)
    expect_equal(payload$events[[1L]]$value, 150)
    expect_identical(payload$events[[1L]]$status, "corrected")
    original_row <- Filter(function(row) {
      identical(row$id, original_id)
    }, payload$eventLedger)[[1L]]
    expect_identical(original_row$status, "superseded")
    expect_false(original_row$amendable)
    expect_match(state$status$text, "original retained")
    expect_true(any(vapply(
      lator_workspace_audit(state$workspace),
      function(item) identical(item$action, "evidence_corrected"),
      logical(1)
    )))

    replacement_id <- payload$events[[1L]]$id
    session$setInputs(liberator_workbench_event = list(
      action = "correct_event", event_id = replacement_id,
      reason = "Dose belonged to another patient",
      actor = "clinician-17", entered_in_error = TRUE, nonce = 5
    ))
    session$flushReact()
    payload <- workbench_payload()
    expect_length(payload$eventLedger, 3L)
    expect_false(any(vapply(
      payload$events, function(row) identical(row$type, "dose"), logical(1)
    )))
    expect_true(any(vapply(
      payload$eventLedger,
      function(row) identical(row$status, "entered_in_error"),
      logical(1)
    )))
  })
})

test_that("endpoint-library choice creates, registers, and selects an endpoint", {
  root <- tempfile("lator-endpoint-gui-")
  app <- lator_gui(
    path = root, passphrase = "endpoint library test passphrase",
    launch.browser = NULL
  )
  server <- app[["serverFuncSource"]]()

  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "new_patient", patient_id = "P-ENDPOINT-001",
      study_id = "TEST", label = "Endpoint test", nonce = 1
    ))
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "add_medication", drug = "Drug A",
      therapeutic_class = "antiseizure",
      configure_endpoint = FALSE, nonce = 2
    ))
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "create_endpoint",
      template_id = "template-aed-range",
      values = list(
        drug = "Drug A", unit = "mg/L", lower = "2", upper = "8",
        source = "Institutional protocol", status = "reviewed",
        version = "1.2.0"
      ),
      nonce = 3
    ))
    session$flushReact()

    original_key <- state$endpoint_id
    expect_match(original_key, "patient-.+-drug-a-aed-drug-a@1.2.0")
    expect_true(original_key %in% names(state$patient_endpoints))
    expect_equal(
      state$patient_endpoints[[state$endpoint_id]]$source,
      "Institutional protocol"
    )
    expect_equal(workbench_payload()$selectedEndpoint, state$endpoint_id)
    expect_match(state$status$text, "Created and selected endpoint")

    restored <- .lator_registered_endpoints(state$workspace)
    expect_length(restored, 0L)
    patient <- lator_patient_get(state$workspace, "P-ENDPOINT-001")
    expect_equal(
      lator_patient_endpoint_get(patient, "Drug A")$endpoint_key,
      original_key
    )

    session$setInputs(liberator_workbench_event = list(
      action = "revise_endpoint",
      original_key = original_key,
      template_id = "template-aed-range",
      values = list(
        drug = "Drug A", unit = "mg/L", lower = "3", upper = "7",
        source = "Individual reviewed target", status = "reviewed",
        version = "1.2.1"
      ),
      nonce = 4
    ))
    session$flushReact()
    revised_key <- state$endpoint_id
    expect_match(revised_key, "patient-.+-drug-a-aed-drug-a@1.2.1")
    expect_equal(
      state$patient_endpoints[[state$endpoint_id]]$rules$lower, 3
    )
    patient <- lator_patient_get(state$workspace, "P-ENDPOINT-001")
    profile <- lator_patient_endpoint_get(patient, "Drug A")
    expect_equal(profile$endpoint_key, revised_key)
    expect_length(profile$endpoint_history, 2L)
    expect_equal(
      state$patient_endpoints[[state$endpoint_id]]$metadata$supersedes_endpoint_key,
      original_key
    )
    expect_match(state$status$text, "revised endpoint")
  })
})

test_that("GUI creates and versions patient-specific multi-endpoint objectives", {
  root <- tempfile("lator-multi-endpoint-gui-")
  app <- lator_gui(
    path = root, passphrase = "multi endpoint gui test passphrase",
    launch.browser = NULL
  )
  server <- app[["serverFuncSource"]]()

  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "new_patient", patient_id = "P-MULTI-ENDPOINT-001",
      study_id = "TEST", label = "Multi endpoint", nonce = 1
    ))
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "add_medication", drug = "Drug A",
      therapeutic_class = "antiseizure",
      configure_endpoint = FALSE, nonce = 2
    ))
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "create_endpoint",
      template_id = "template-aed-range",
      values = list(
        drug = "Drug A", unit = "mg/L", lower = "2", upper = "8",
        source = "Efficacy protocol", status = "reviewed",
        version = "1.0.0"
      ),
      nonce = 3
    ))
    session$flushReact()
    efficacy_key <- state$endpoint_id

    session$setInputs(liberator_workbench_event = list(
      action = "create_endpoint",
      template_id = "template-tacrolimus",
      values = list(
        drug = "Drug A", unit = "mg/L", lower = "1", upper = "10",
        source = "Safety protocol", status = "reviewed",
        version = "1.0.0"
      ),
      nonce = 4
    ))
    session$flushReact()
    safety_key <- state$endpoint_id
    expect_true(all(
      c(efficacy_key, safety_key) %in% names(state$patient_endpoints)
    ))

    components <- list(
      list(
        endpoint_key = efficacy_key, role = "primary", weight = 2,
        hard_constraint = FALSE, minimum_attainment = 0.9
      ),
      list(
        endpoint_key = safety_key, role = "safety", weight = 1,
        hard_constraint = TRUE, minimum_attainment = 0.8
      )
    )
    session$setInputs(liberator_workbench_event = list(
      action = "create_endpoint_set",
      name = "Drug A benefit-risk objective",
      source = "Combined institutional protocol",
      status = "reviewed", version = "1.0.0",
      components = components, nonce = 5
    ))
    session$flushReact()

    objective_key <- state$endpoint_id
    objective <- state$patient_endpoints[[objective_key]]
    expect_identical(objective$kind, "multi_endpoint")
    expect_length(objective$rules$components, 2L)
    payload <- workbench_payload()
    selected <- Filter(function(item) {
      identical(item$id, objective_key)
    }, payload$endpoints)[[1L]]
    expect_true(selected$isSet)
    expect_length(selected$components, 2L)
    expect_identical(payload$endpointEdit$kind, "multi_endpoint")
    expect_equal(payload$endpointEdit$version, "1.0.1")

    session$setInputs(liberator_workbench_event = list(
      action = "revise_endpoint_set",
      original_key = objective_key,
      name = "Drug A benefit-risk objective",
      source = "Revised combined institutional protocol",
      status = "reviewed", version = "1.0.1",
      components = components, nonce = 6
    ))
    session$flushReact()
    revised <- state$patient_endpoints[[state$endpoint_id]]
    expect_identical(revised$kind, "multi_endpoint")
    expect_equal(revised$version, "1.0.1")
    expect_identical(
      revised$metadata$supersedes_endpoint_key, objective_key
    )
    patient <- lator_patient_get(
      state$workspace, "P-MULTI-ENDPOINT-001"
    )
    expect_length(
      lator_patient_endpoint_get(
        patient, "Drug A"
      )$endpoint_history,
      4L
    )
  })
})

test_that("medication switching restores each drug-specific endpoint", {
  root <- tempfile("lator-multidrug-gui-")
  app <- lator_gui(
    path = root, passphrase = "multi medication test passphrase",
    launch.browser = NULL
  )
  server <- app[["serverFuncSource"]]()

  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "new_patient", patient_id = "P-MULTI-001",
      study_id = "TEST", label = "Multi medication", nonce = 1
    ))
    session$setInputs(liberator_workbench_event = list(
      action = "add_medication", drug = "phenytoin",
      therapeutic_class = "antiseizure",
      configure_endpoint = TRUE, nonce = 2
    ))
    session$flushReact()
    expect_equal(state$drug_id, "phenytoin")
    expect_gt(state$endpoint_prompt, 0L)
    session$setInputs(liberator_workbench_event = list(
      action = "create_endpoint", template_id = "template-aed-range",
      values = list(
        drug = "phenytoin", lower = "10", upper = "20", unit = "mg/L",
        source = "Reviewed local protocol", status = "reviewed",
        version = "1.0.0"
      ), nonce = 3
    ))
    session$flushReact()
    phenytoin_endpoint <- state$endpoint_id

    session$setInputs(liberator_workbench_event = list(
      action = "add_medication", drug = "warfarin",
      therapeutic_class = "vitamin-k-antagonist",
      configure_endpoint = TRUE, nonce = 4
    ))
    session$flushReact()
    expect_equal(state$drug_id, "warfarin")
    expect_null(state$endpoint_id)
    session$setInputs(liberator_workbench_event = list(
      action = "create_endpoint", template_id = "template-warfarin",
      values = list(
        drug = "warfarin", lower = "2", upper = "3",
        target_fraction = "0.65", source = "Reviewed local protocol",
        status = "reviewed", version = "1.0.0"
      ), nonce = 5
    ))
    session$flushReact()
    warfarin_endpoint <- state$endpoint_id
    expect_false(identical(phenytoin_endpoint, warfarin_endpoint))

    session$setInputs(liberator_workbench_event = list(
      action = "select_drug", id = "phenytoin", nonce = 6
    ))
    session$flushReact()
    expect_equal(state$endpoint_id, phenytoin_endpoint)

    session$setInputs(liberator_workbench_event = list(
      action = "select_drug", id = "warfarin", nonce = 7
    ))
    session$flushReact()
    expect_equal(state$endpoint_id, warfarin_endpoint)
    expect_setequal(
      vapply(workbench_payload()$medications, `[[`, character(1), "key"),
      c("phenytoin", "warfarin")
    )
  })
})

test_that("adding treatment medication can hand off to endpoint selection", {
  root <- tempfile("lator-add-medication-gui-")
  app <- lator_gui(
    path = root, passphrase = "add medication test passphrase",
    launch.browser = NULL
  )
  server <- app[["serverFuncSource"]]()

  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "new_patient", patient_id = "P-TREATMENT-GUI",
      study_id = "TEST", label = "Treatment profile", nonce = 1
    ))
    session$flushReact()
    initial_prompt <- state$endpoint_prompt
    session$setInputs(liberator_workbench_event = list(
      action = "add_medication", drug = "phenytoin",
      therapeutic_class = "", configure_endpoint = TRUE, nonce = 2
    ))
    session$flushReact()

    expect_equal(state$drug_id, "phenytoin")
    expect_null(state$endpoint_id)
    expect_gt(state$endpoint_prompt, initial_prompt)
    expect_length(workbench_payload()$events, 0L)
    medication <- workbench_payload()$medications[[1L]]
    expect_equal(medication$drug, "phenytoin")
    expect_equal(medication$therapeutic_class, "antiseizure")
    suggested <- Filter(
      function(template) isTRUE(template$recommended),
      workbench_payload()$endpointTemplates
    )
    expect_length(suggested, 1L)
    expect_equal(suggested[[1L]]$id, "template-aed-range")
    defaults <- stats::setNames(
      lapply(suggested[[1L]]$fields, `[[`, "default"),
      vapply(suggested[[1L]]$fields, `[[`, character(1), "name")
    )
    expect_equal(defaults$lower, 10)
    expect_equal(defaults$upper, 20)

    prompt_after_phenytoin <- state$endpoint_prompt
    session$setInputs(liberator_workbench_event = list(
      action = "add_medication", drug = "warfarin",
      therapeutic_class = "", configure_endpoint = FALSE, nonce = 3
    ))
    session$flushReact()
    expect_equal(state$drug_id, "warfarin")
    expect_equal(state$endpoint_prompt, prompt_after_phenytoin)
    patient <- lator_patient_get(state$workspace, "P-TREATMENT-GUI")
    expect_setequal(
      lator_patient_medications(patient)$key,
      c("phenytoin", "warfarin")
    )
  })
})

test_that("endpoint template versions can be assigned independently to patients", {
  app <- lator_gui(
    path = tempfile("lator-patient-endpoints-"),
    passphrase = "patient endpoint isolation passphrase",
    launch.browser = NULL
  )
  server <- app[["serverFuncSource"]]()

  shiny::testServer(server, {
    create_patient_endpoint <- function(patient_id, nonce) {
      session$setInputs(liberator_workbench_event = list(
        action = "new_patient", patient_id = patient_id,
        study_id = "TEST", label = patient_id, nonce = nonce
      ))
      session$flushReact()
      session$setInputs(liberator_workbench_event = list(
        action = "add_medication", drug = "lamotrigine",
        therapeutic_class = "antiseizure",
        configure_endpoint = FALSE, nonce = nonce + 1
      ))
      session$flushReact()
      session$setInputs(liberator_workbench_event = list(
        action = "create_endpoint", template_id = "template-aed-range",
        values = list(
          drug = "lamotrigine", lower = "3", upper = "15",
          unit = "mg/L", source = "Reviewed system template",
          status = "reviewed", version = "1.0.0"
        ), nonce = nonce + 2
      ))
      session$flushReact()
      state$endpoint_id
    }

    first_key <- create_patient_endpoint("PATIENT-ENDPOINT-A", 1)
    second_key <- create_patient_endpoint("PATIENT-ENDPOINT-B", 10)
    expect_false(identical(first_key, second_key))
    expect_match(state$status$text, "Created and selected endpoint")
    expect_length(.lator_registered_endpoints(state$workspace), 0L)
  })
})

test_that("dose and TDM evidence are constrained to patient treatments", {
  app <- lator_gui(
    path = tempfile("lator-treatment-evidence-"),
    passphrase = "treatment evidence constraint passphrase",
    launch.browser = NULL
  )
  server <- app[["serverFuncSource"]]()

  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "new_patient", patient_id = "P-EVIDENCE",
      study_id = "TEST", label = "Evidence", nonce = 1
    ))
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "add_medication", drug = "carbamazepine",
      therapeutic_class = "antiseizure",
      monitoring_analytes = "carbamazepine-10,11-epoxide",
      configure_endpoint = FALSE, nonce = 2
    ))
    session$flushReact()

    session$setInputs(liberator_workbench_event = list(
      action = "add_event", type = "dose", time = "0",
      treatment_drug = "phenytoin", name = "phenytoin",
      value = "100", unit = "mg", nonce = 3
    ))
    session$flushReact()
    expect_length(workbench_payload()$events, 0L)
    expect_match(state$status$text, "medication already added")

    session$setInputs(liberator_workbench_event = list(
      action = "add_event", type = "dose", time = "0",
      treatment_drug = "carbamazepine", name = "carbamazepine",
      value = "200", unit = "mg", nonce = 4
    ))
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "add_event", type = "concentration", time = "2",
      treatment_drug = "carbamazepine",
      name = "carbamazepine-10,11-epoxide",
      value = "1.4", unit = "mg/L", nonce = 5
    ))
    session$flushReact()
    patient <- lator_patient_get(state$workspace, "P-EVIDENCE")
    expect_length(patient$events, 2L)
    expect_equal(patient$events[[2L]]$name, "carbamazepine-10,11-epoxide")
    expect_equal(patient$events[[2L]]$metadata$drug, "carbamazepine")
  })
})

test_that("patient deletion is available through the hosted workbench", {
  app <- lator_gui(
    path = tempfile("lator-delete-gui-"),
    passphrase = "patient deletion GUI passphrase",
    launch.browser = NULL
  )
  server <- app[["serverFuncSource"]]()

  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "new_patient", patient_id = "P-DELETE-GUI",
      study_id = "TEST", label = "Delete me", nonce = 1
    ))
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "delete_patient", confirmation = "YES", nonce = 2
    ))
    session$flushReact()
    expect_null(state$patient_id)
    expect_false(
      "P-DELETE-GUI" %in% lator_patient_list(state$workspace)$patient_id
    )
    expect_match(state$status$text, "deleted", ignore.case = TRUE)
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
    expect_true(all(c(
      "aed-example-aed", "teaching-aed-trough-safety",
      "teaching-aed-benefit-risk"
    ) %in% names(state$endpoints)))
    payload <- workbench_payload()
    expect_length(payload$events, 9L)
    expect_identical(payload$selectedEndpoint, "teaching-aed-benefit-risk")
    expect_identical(payload$endpointEdit$kind, "multi_endpoint")
    expect_equal(payload$regimenDefaults$amounts,
                 c(150, 225, 300, 375, 450))
    expect_equal(payload$regimenDefaults$intervals, c(12, 24))
    expect_match(state$status$text, "synthetic teaching patient", ignore.case = TRUE)
  })
})
