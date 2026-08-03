lator_test_qualification <- function(
    id, route = "oral", weight = c(40, 120), evidence = 0.9) {
  list(
    id = id, name = paste("Candidate", id), source = "LibeRary",
    model_version = "1.0.0", model_hash = paste0("hash-", id),
    qualification = list(
      schema = "liberary.clinical_qualification",
      schema_version = "1.0.0",
      qualification_id = paste0("cq-", id),
      status = "qualified",
      scope = list(
        drugs = "Drug A", indications = "epilepsy", routes = route,
        formulations = character(), regimens = character(),
        endpoint_ids = character(), endpoint_kinds = "therapeutic_range",
        population = list(
          WT = list(min = weight[1L], max = weight[2L], unit = "kg",
                    required = TRUE, hard = TRUE, covariate = "WT")
        ),
        covariates = list(
          required = "WT", optional = "AGE", ranges = list()
        ),
        assays = list(required = TRUE, matrices = "plasma",
                      methods = character(), units = "mg/L")
      ),
      evidence = list(external_validation_score = evidence),
      governance = list(
        issuer = "Hospital", reviewer = "Committee",
        review_due = "2099-12-31T00:00:00Z"
      ),
      limitations = character()
    )
  )
}

lator_selection_patient <- function(weight = 70) {
  patient <- lator_patient_new(
    "SELECT-001", metadata = list(indication = "epilepsy")
  )
  patient <- lator_patient_add_event(
    patient, "covariate", 0, "WT", weight, "kg"
  )
  patient <- lator_patient_add_event(
    patient, "dose", 0, "Drug A", 100, "mg",
    metadata = list(route = "oral", formulation = "tablet")
  )
  lator_patient_add_event(
    patient, "concentration", 2, "Drug A", 5, "mg/L",
    metadata = list(matrix = "plasma", assay = "LC-MS/MS")
  )
}

test_that("qualified model selection gates before ranking and is reproducible", {
  endpoint <- lator_endpoint_aed(
    "Drug A", 2, 8, "mg/L", "Institutional protocol",
    status = "qualified",
    metadata = list(
      research_acknowledged = TRUE,
      qualification_attestation = list(
        issuer = "Fixture governance board", reviewer = "Fixture reviewer",
        reviewed_at = "2026-08-02T00:00:00Z",
        evidence = "fixture-protocol-v1",
        scope = "Synthetic model-selection test"
      )
    )
  )
  candidates <- list(
    lator_test_qualification("oral"),
    lator_test_qualification("iv", route = "intravenous", evidence = 1)
  )
  selection <- lator_model_select(
    lator_selection_patient(), endpoint, candidates
  )
  expect_s3_class(selection, "lator_model_selection")
  expect_equal(selection$status, "selected")
  expect_equal(selection$selected_model_id, "oral")
  expect_true(nzchar(selection$selection_hash))
  expect_false(selection$candidates[[2L]]$eligible)
  expect_true(any(grepl(
    "routes_outside_qualification",
    unlist(lapply(selection$candidates, `[[`, "blockers"))
  )))
})

test_that("selection fails closed outside all qualified populations", {
  endpoint <- lator_endpoint_aed(
    "Drug A", 2, 8, "mg/L", "Institutional protocol"
  )
  selection <- lator_model_select(
    lator_selection_patient(weight = 25), endpoint,
    list(lator_test_qualification("adult"))
  )
  expect_equal(selection$status, "no_suitable_model")
  expect_false(nzchar(selection$selected_model_id))
  expect_true(any(grepl(
    "outside_validated_range:WT", selection$candidates[[1L]]$blockers,
    fixed = TRUE
  )))
})

test_that("population ranges default to their named covariate", {
  endpoint <- lator_endpoint_aed(
    "Drug A", 2, 8, "mg/L", "Institutional protocol"
  )
  candidate <- lator_test_qualification("named-range")
  candidate$qualification$scope$population$WT$covariate <- ""
  selection <- lator_model_select(
    lator_selection_patient(), endpoint, list(candidate)
  )
  expect_equal(selection$status, "selected")
  expect_false(any(grepl(
    "domain_covariate_missing:", selection$candidates[[1L]]$blockers,
    fixed = TRUE
  )))
})

test_that("near-tied suitable candidates withhold automatic selection", {
  endpoint <- lator_endpoint_aed(
    "Drug A", 2, 8, "mg/L", "Institutional protocol"
  )
  selection <- lator_model_select(
    lator_selection_patient(), endpoint,
    list(
      lator_test_qualification("one", evidence = 0.90),
      lator_test_qualification("two", evidence = 0.89)
    ),
    minimum_margin = 0.05
  )
  expect_equal(selection$status, "multiple_suitable_models")
  expect_false(nzchar(selection$selected_model_id))
})

test_that("model-selection decisions are retained on the encrypted patient", {
  workspace <- lator_workspace(
    tempfile("lator-selection-"), "model selection test passphrase"
  )
  patient <- lator_patient_save(workspace, lator_selection_patient())
  endpoint <- lator_endpoint_aed("Drug A", 2, 8, "mg/L", "Protocol")
  selection <- lator_model_select(
    patient, endpoint, list(lator_test_qualification("oral"))
  )
  saved <- lator_model_selection_save(workspace, patient, selection)
  expect_length(saved$model_selections, 1L)
  expect_equal(saved$model_selections[[1L]]$selection_hash,
               selection$selection_hash)
  expect_true(isTRUE(attr(lator_workspace_audit(workspace), "valid")))
})

test_that("multi-endpoint model selection requires qualification for every component", {
  qualification_metadata <- list(
    research_acknowledged = TRUE,
    qualification_attestation = list(
      issuer = "Fixture governance board", reviewer = "Fixture reviewer",
      reviewed_at = "2026-08-02T00:00:00Z", evidence = "fixture-protocol-v1",
      scope = "Synthetic model-selection test"
    )
  )
  efficacy <- lator_endpoint_aed(
    "Drug A", 2, 8, "mg/L", "Institutional protocol",
    status = "qualified", metadata = qualification_metadata
  )
  safety <- lator_endpoint(
    id = "drug-a-safety", name = "Drug A safety trough",
    drug = "Drug A", kind = "trough_range", metric = "trough",
    unit = "mg/L", rules = list(lower = 1, upper = 10, target = 5.5),
    source = "Institutional protocol", status = "qualified",
    metadata = qualification_metadata
  )
  objective <- lator_endpoint_set(
    id = "drug-a-benefit-risk", name = "Drug A benefit-risk",
    drug = "Drug A", source = "Institutional protocol",
    status = "qualified", metadata = qualification_metadata,
    components = list(
      lator_endpoint_component(efficacy, role = "primary"),
      lator_endpoint_component(safety, role = "safety")
    )
  )

  incomplete <- lator_test_qualification("incomplete")
  blocked <- lator_model_select(
    lator_selection_patient(), objective, list(incomplete)
  )
  expect_equal(blocked$status, "no_suitable_model")
  expect_true(
    "endpoint_outside_qualification" %in%
      blocked$candidates[[1L]]$blockers
  )

  complete <- lator_test_qualification("complete")
  complete$qualification$scope$endpoint_kinds <- c(
    "therapeutic_range", "trough_range"
  )
  selected <- lator_model_select(
    lator_selection_patient(), objective, list(complete)
  )
  expect_equal(selected$status, "selected")
  expect_equal(selected$selected_model_id, "complete")
})
