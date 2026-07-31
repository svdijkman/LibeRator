test_that("patient medication profiles are isolated by drug", {
  patient <- lator_patient_new("P-MULTI-001")
  patient <- lator_patient_add_event(
    patient, "dose", 0, "phenytoin", 300, "mg",
    metadata = list(therapeutic_class = "antiseizure")
  )
  patient <- lator_patient_add_event(
    patient, "dose", 4, "warfarin", 5, "mg",
    metadata = list(therapeutic_class = "vitamin-k-antagonist")
  )
  medications <- lator_patient_medications(patient)
  expect_setequal(medications$key, c("phenytoin", "warfarin"))
  expect_equal(
    medications$therapeutic_class[medications$key == "warfarin"],
    "vitamin-k-antagonist"
  )

  phenytoin <- lator_endpoint_aed(
    "phenytoin", 10, 20, "mg/L", "Reviewed local protocol"
  )
  patient <- lator_patient_endpoint_set(
    patient, "phenytoin", "aed-phenytoin@1.0.0", phenytoin,
    therapeutic_class = "antiseizure"
  )
  expect_equal(
    lator_patient_endpoint_get(patient, "phenytoin")$endpoint_key,
    "aed-phenytoin@1.0.0"
  )
  expect_null(lator_patient_endpoint_get(patient, "warfarin"))
  expect_error(
    lator_patient_endpoint_set(
      patient, "warfarin", "aed-phenytoin@1.0.0", phenytoin
    ),
    "does not match"
  )
})

test_that("medications can be added before any dose evidence exists", {
  patient <- lator_patient_new("P-TREATMENT-001")
  patient <- lator_patient_medication_add(
    patient, "phenytoin", "antiseizure",
    monitoring_analytes = "free phenytoin"
  )
  medications <- lator_patient_medications(patient)
  expect_equal(nrow(medications), 1L)
  expect_equal(medications$drug, "phenytoin")
  expect_equal(medications$therapeutic_class, "antiseizure")
  expect_equal(medications$monitoring_analytes, "free phenytoin")
  expect_true(is.na(medications$last_dose_time))
  expect_equal(medications$treatment_status, "active")
  expect_length(patient$events, 0L)
  expect_error(
    lator_patient_medication_add(patient, "phenytoin"),
    "already in this patient profile"
  )

  patient <- lator_patient_add_event(
    patient, "dose", 12, "phenytoin", 300, "mg"
  )
  medications <- lator_patient_medications(patient)
  expect_equal(nrow(medications), 1L)
  expect_equal(medications$last_dose_time, 12)
})

test_that("population-model preferences persist per patient medication", {
  patient <- lator_patient_medication_add(
    lator_patient_new("P-MODEL-PREFERENCE"), "lamotrigine", "antiseizure"
  )
  patient <- .lator_patient_model_set(
    patient, "lamotrigine", "rivas-lamotrigine", "model-definition-hash"
  )
  expect_equal(
    lator_patient_medications(patient)$model_id, "rivas-lamotrigine"
  )
  expect_equal(
    .lator_patient_model_get(patient, "lamotrigine")$model_hash,
    "model-definition-hash"
  )

  endpoint <- lator_endpoint_aed(
    "lamotrigine", 3, 15, "mg/L", "Reviewed system template"
  )
  patient <- lator_patient_endpoint_set(
    patient, "lamotrigine", "aed-lamotrigine@1.0.0", endpoint
  )
  expect_equal(
    .lator_patient_model_get(patient, "lamotrigine")$model_id,
    "rivas-lamotrigine"
  )
})

test_that("endpoint snapshots belong to patient-medication assignments", {
  endpoint <- lator_endpoint_aed(
    "lamotrigine", 3, 15, "mg/L", "Reviewed system template"
  )
  first <- lator_patient_medication_add(
    lator_patient_new("PATIENT-ENDPOINT-A"), "lamotrigine", "antiseizure"
  )
  second <- lator_patient_medication_add(
    lator_patient_new("PATIENT-ENDPOINT-B"), "lamotrigine", "antiseizure"
  )
  first_key <- .lator_patient_endpoint_instance_key(
    first, "lamotrigine", endpoint
  )
  second_key <- .lator_patient_endpoint_instance_key(
    second, "lamotrigine", endpoint
  )
  first <- lator_patient_endpoint_set(
    first, "lamotrigine", first_key, endpoint
  )
  second <- lator_patient_endpoint_set(
    second, "lamotrigine", second_key, endpoint
  )

  expect_false(identical(first_key, second_key))
  expect_equal(
    lator_patient_endpoint_get(first, "lamotrigine")$endpoint_snapshot$rules,
    endpoint$rules
  )
  expect_equal(
    lator_patient_endpoint_get(second, "lamotrigine")$endpoint_snapshot$rules,
    endpoint$rules
  )
})

test_that("revised targets remain specific to the patient-drug profile", {
  original <- lator_endpoint_aed(
    "phenytoin", 10, 20, "mg/L", "Original reviewed protocol"
  )
  revised <- lator_endpoint_aed(
    "phenytoin", 8, 16, "mg/L", "Individual reviewed target"
  )
  revised$version <- "1.0.1"
  patient_a <- lator_patient_add_event(
    lator_patient_new("PATIENT-A"), "dose", 0, "phenytoin", 300, "mg"
  )
  patient_b <- lator_patient_add_event(
    lator_patient_new("PATIENT-B"), "dose", 0, "phenytoin", 300, "mg"
  )
  patient_a <- lator_patient_endpoint_set(
    patient_a, "phenytoin", "aed-phenytoin@1.0.0", original
  )
  patient_b <- lator_patient_endpoint_set(
    patient_b, "phenytoin", "aed-phenytoin@1.0.0", original
  )
  patient_a <- lator_patient_endpoint_set(
    patient_a, "phenytoin", "aed-phenytoin@1.0.1", revised
  )

  profile_a <- lator_patient_endpoint_get(patient_a, "phenytoin")
  profile_b <- lator_patient_endpoint_get(patient_b, "phenytoin")
  expect_equal(profile_a$endpoint_key, "aed-phenytoin@1.0.1")
  expect_equal(profile_b$endpoint_key, "aed-phenytoin@1.0.0")
  expect_length(profile_a$endpoint_history, 2L)
  expect_length(profile_b$endpoint_history, 1L)
})

test_that("assessment evidence is restricted to the endpoint medication", {
  patient <- lator_patient_new("P-MULTI-002")
  patient <- lator_patient_add_event(
    patient, "dose", 0, "Drug A", 100, "mg"
  )
  patient <- lator_patient_add_event(
    patient, "concentration", 2, "Drug A", 4, "mg/L"
  )
  patient <- lator_patient_add_event(
    patient, "dose", 0, "warfarin", 5, "mg"
  )
  patient <- lator_patient_add_event(
    patient, "concentration", 2, "warfarin", 2.5, "INR"
  )

  drug_a <- .lator_match_therapy_events(patient, "concentration", "Drug A")
  warfarin <- .lator_match_therapy_events(
    patient, "concentration", "warfarin"
  )
  expect_length(drug_a, 1L)
  expect_length(warfarin, 1L)
  expect_equal(drug_a[[1L]]$value, 4)
  expect_equal(warfarin[[1L]]$value, 2.5)
  expect_length(
    .lator_match_therapy_events(patient, "concentration", "unrecorded drug"),
    0L
  )
})

test_that("related-analyte TDM is not silently treated as parent-drug DV", {
  patient <- lator_patient_medication_add(
    lator_patient_new("P-METABOLITE"), "carbamazepine", "antiseizure",
    monitoring_analytes = "carbamazepine-10,11-epoxide"
  )
  patient <- lator_patient_add_event(
    patient, "concentration", 2, "carbamazepine", 6, "mg/L",
    metadata = list(
      drug = "carbamazepine", analyte = "carbamazepine"
    )
  )
  patient <- lator_patient_add_event(
    patient, "concentration", 2,
    "carbamazepine-10,11-epoxide", 1.4, "mg/L",
    metadata = list(
      drug = "carbamazepine",
      analyte = "carbamazepine-10,11-epoxide"
    )
  )

  parent <- .lator_match_therapy_events(
    patient, "concentration", "carbamazepine"
  )
  metabolite <- .lator_match_therapy_events(
    patient, "concentration", "carbamazepine-10,11-epoxide"
  )
  expect_length(parent, 1L)
  expect_equal(parent[[1L]]$value, 6)
  expect_length(metabolite, 1L)
  expect_equal(metabolite[[1L]]$value, 1.4)
})
