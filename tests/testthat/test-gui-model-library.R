test_that("medication catalogue matching is exact and alias aware", {
  expect_true(.lator_compound_matches_drug("lamotrigine", "Lamotrigine"))
  expect_true(.lator_compound_matches_drug("valproic acid", "sodium valproate"))
  expect_true(.lator_compound_matches_drug("phenobarbitone", "phenobarbital"))
  expect_false(.lator_compound_matches_drug("carbamazepine", "oxcarbazepine"))
  expect_false(.lator_compound_matches_drug("warfarin", "lamotrigine"))
})

test_that("manual model catalogue returns only usable models for the drug", {
  catalogue <- data.frame(
    library_id = c("lam-review", "warfarin-validated", "lam-stub"),
    title = c("Lamotrigine model", "Warfarin model", "Incomplete lamotrigine"),
    status = c("review", "validated", "stub"),
    compound = c("lamotrigine", "warfarin", "lamotrigine"),
    population = c("Adults", "Adults", "Unknown"),
    advan = c(2L, 1L, NA_integer_),
    trans = c(2L, 2L, NA_integer_),
    model_type = "pk",
    version = "1.0.0",
    confidence_overall = c(0.9, 0.95, 0.2),
    clinical_status = c("candidate", "qualified", ""),
    clinically_qualified = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    .lator_liberary_api = function(name, minimum_version = "0.7.10") {
      expect_equal(name, "library_list")
      function(...) catalogue
    },
    .package = "LibeRator"
  )

  rows <- .lator_liberary_models_for_drug("lamotrigine")
  expect_length(rows, 1L)
  expect_equal(rows[[1L]]$id, "lam-review")
  expect_true(rows[[1L]]$researchAcknowledgementRequired)
  expect_false(rows[[1L]]$clinicallyQualified)
})

test_that("manual model creation reuses LibeRation templates", {
  catalogue <- .lator_model_templates_for_gui()
  expect_equal(catalogue[[1L]]$id, "standard_advan")
  expect_setequal(
    vapply(catalogue[-1L], `[[`, character(1), "id"),
    LibeRation::nm_structural_templates()$template
  )

  standard <- .lator_model_from_template_event(list(
    template_type = "advan", name = "Oral teaching model",
    advan = 2, trans = 2
  ))
  expect_s3_class(standard, "nm_model")
  expect_equal(standard$ADVAN, 2L)
  expect_equal(attr(standard, "name", exact = TRUE), "Oral teaching model")

  advanced <- .lator_model_from_template_event(list(
    template_type = "structural", template_id = "indirect_response",
    name = "PD teaching model", iiv = TRUE, residual = "combined"
  ))
  expect_s3_class(advanced, "nm_model")
  expect_equal(attr(advanced, "template", exact = TRUE), "indirect_response")
  expect_equal(attr(advanced, "name", exact = TRUE), "PD teaching model")
})

test_that("model information separates parameters and derived quantities", {
  model <- LibeRation::nm_model(
    INPUT = c(
      "ID", "TIME", "EVID", "AMT", "CMT", "DV", "MDV", "WT",
      "COMED_PHT", "COMED_PHB", "COMED_CBZ"
    ),
    ADVAN = 2, TRANS = 2,
    PRED = paste(
      "NIND=COMED_PHT+COMED_PHB+COMED_CBZ",
      "IND=ifelse(NIND>1,1,0)",
      "CL=THETA(1)*WT*exp(THETA(2)*IND+ETA(1))",
      "V=THETA(3)*WT", "KA=THETA(4)", "S2=V", sep = "\n"
    ),
    ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(THETA = 1:4, Value = c(0.03, 0.8, 1.5, 1.3)),
    OMEGAS = data.frame(OMEGA = 1L, Value = 0.1),
    SIGMAS = data.frame(SIGMA = 1L, Value = 1.2),
    COVARIATES = c("WT", "COMED_PHT", "COMED_PHB", "COMED_CBZ")
  )
  attr(model, "name") <- "Example lamotrigine model"
  attr(model, "lator_parameter_labels") <- list(
    theta = c("CL/F per kg", "Multiple-inducer effect", "V/F per kg", "KA")
  )
  attr(model, "library_provenance") <- list(
    source = "LibeRary", library_id = "example-lamotrigine",
    library_version = "1.0.0", status_at_import = "review",
    model_metadata = list(mapping_review_required = TRUE),
    study = list(population = "Adults", route = "oral"),
    qualification = list(clinical_use = list(list(
      status = "candidate", limitations = "Independent review required.",
      evidence = list(clinical_validation = "not performed")
    )))
  )

  info <- .lator_model_info_for_gui(model, "example-lamotrigine")
  expect_match(info$structure, "first-order extravascular absorption")
  expect_equal(info$parameters[[1L]]$description, "CL/F per kg")
  expect_setequal(
    vapply(info$derived, `[[`, character(1), "name"),
    c("NIND", "IND")
  )
  expect_match(
    info$derived[[match(
      "NIND", vapply(info$derived, `[[`, character(1), "name")
    )]]$description,
    "Count of"
  )
  expect_true(any(grepl("not performed", info$limitations)))
  expect_true(any(grepl("correctly encoded covariates", info$limitations)))

  labels <- .lator_control_parameter_labels(c(
    "$THETA", "(0, 0.03) ; CL/F per kg", "(0, 1.5) ; V/F per kg",
    "$OMEGA", "0.1"
  ))
  expect_equal(labels, c("CL/F per kg", "V/F per kg"))
})

test_that("medication-scoped models do not leak into another drug selector", {
  workspace <- lator_workspace(
    tempfile("lator-model-scope-"),
    "model medication scope test passphrase"
  )
  patient <- lator_patient_new("P-SCOPE-001")
  patient <- lator_patient_medication_add(patient, "lamotrigine")
  lator_patient_save(workspace, patient)
  lamotrigine <- LibeRation::nm_advan_template(2L)
  warfarin <- LibeRation::nm_advan_template(1L)
  unscoped <- LibeRation::nm_advan_template(1L)
  attr(lamotrigine, "lator_medications") <- "lamotrigine"
  attr(warfarin, "lator_medications") <- "warfarin"

  payload <- .lator_gui_payload(
    workspace, "P-SCOPE-001",
    models = list(
      lamotrigine = lamotrigine, warfarin = warfarin,
      supplied_unscoped = unscoped
    ),
    selected_drug = "lamotrigine"
  )
  expect_setequal(
    vapply(payload$models, `[[`, character(1), "id"),
    c("lamotrigine", "supplied_unscoped")
  )
})

test_that("GUI template creation registers and selects the model", {
  app <- lator_gui(
    path = tempfile("lator-model-template-"),
    passphrase = "manual model template test passphrase",
    launch.browser = NULL
  )
  server <- app[["serverFuncSource"]]()

  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "new_patient", patient_id = "P-MODEL-001",
      label = "Model test", nonce = 1
    ))
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "add_medication", drug = "lamotrigine",
      configure_endpoint = FALSE, nonce = 2
    ))
    session$flushReact()
    session$setInputs(liberator_workbench_event = list(
      action = "model_create_template", template_type = "advan",
      name = "Lamotrigine teaching model", advan = 2, trans = 2,
      nonce = 3
    ))
    session$flushReact()

    expect_true(nzchar(state$model_id))
    expect_s3_class(state$models[[state$model_id]], "nm_model")
    expect_equal(
      attr(state$models[[state$model_id]], "name", exact = TRUE),
      "Lamotrigine teaching model"
    )
    expect_match(state$status$text, "LibeRation template", fixed = TRUE)
    expect_equal(
      LibeRator:::.lator_model_get(
        state$workspace, state$model_id
      )$qualification$status,
      "research"
    )
    stored <- lator_patient_get(state$workspace, "P-MODEL-001")
    preference <- .lator_patient_model_get(stored, "lamotrigine")
    expect_equal(preference$model_id, state$model_id)
    expect_true(nzchar(preference$model_hash))

    selected <- state$model_id
    state$model_id <- NULL
    restore_model_selection("P-MODEL-001")
    expect_equal(state$model_id, selected)
  })
})
