test_that("AED endpoints target the supplied range midpoint", {
  endpoint <- lator_endpoint_aed("Drug A", 10, 20, "mg/L", "teaching source")
  predictions <- data.frame(TIME = 0:2, IPRED = c(14, 15, 16))
  evaluated <- lator_endpoint_evaluate(endpoint, predictions)
  expect_equal(evaluated$median_metric, 15)
  expect_equal(evaluated$attainment_probability, 1)
  expect_equal(endpoint$rules$target, 15)
})

test_that("beta-lactam endpoint resolves longitudinal MIC", {
  patient <- lator_patient_new("P001")
  patient <- lator_patient_add_event(patient, "covariate", 0, "MIC", 2, "mg/L")
  endpoint <- lator_endpoint_beta_lactam("Example beta-lactam", 0.4, source = "teaching source")
  predictions <- data.frame(TIME = c(0, 1, 2), IPRED = c(4, 2, 0))
  evaluated <- lator_endpoint_evaluate(endpoint, predictions, patient)
  expect_equal(evaluated$median_metric, 0.5, tolerance = 1e-12)
  expect_equal(evaluated$attainment_probability, 1)
})

test_that("ATG endpoints validate explicit pre-event windows", {
  targets <- data.frame(window_start = c(-24, -6), window_end = c(-18, 0),
                        lower = c(1, 2), upper = c(3, 4))
  endpoint <- lator_endpoint_atg("ATG", targets, "AU", "teaching source")
  expect_s3_class(endpoint, "lator_endpoint")
  expect_error(lator_endpoint_atg("ATG", transform(targets, lower = 9), "AU", "x"), "invalid")
})

test_that("the endpoint library exposes ten clinically distinct families", {
  library <- lator_endpoint_library()
  expect_equal(nrow(library), 10L)
  expect_true(all(nzchar(library$constructor)))
  expect_true(all(c(
    "lator_endpoint_vancomycin", "lator_endpoint_aminoglycoside",
    "lator_endpoint_tacrolimus", "lator_endpoint_mycophenolate",
    "lator_endpoint_busulfan", "lator_endpoint_methotrexate",
    "lator_endpoint_warfarin"
  ) %in% library$constructor))
})

test_that("endpoint-library templates expose fields and instantiate definitions", {
  templates <- .lator_endpoint_templates_for_gui()
  expect_length(templates, 10L)
  expect_true(all(vapply(
    templates, function(template) length(template$fields) > 0L, logical(1)
  )))

  endpoint <- .lator_endpoint_from_template(
    "template-aed-range",
    list(
      drug = "Drug A", lower = 2, upper = 8, unit = "mg/L",
      source = "Institutional protocol", status = "reviewed",
      version = "2.1.0"
    )
  )
  expect_s3_class(endpoint, "lator_endpoint")
  expect_equal(endpoint$version, "2.1.0")
  expect_equal(endpoint$rules$lower, 2)

  timed <- .lator_endpoint_from_template(
    "template-methotrexate",
    list(
      drug = "methotrexate", unit = "micromol/L",
      targets = "24,-Inf,10,1\n48,-Inf,1,1",
      source = "Institutional protocol", status = "draft",
      version = "1.0.0"
    )
  )
  expect_equal(nrow(timed$rules$targets), 2L)
  expect_equal(timed$rules$targets$hours_after_dose, c(24, 48))
  expect_error(
    .lator_endpoint_from_template(
      "template-atg-pre-event",
      list(
        drug = "ATG", unit = "AU", targets = "-24,-18,1",
        source = "Protocol", status = "draft", version = "1.0.0"
      )
    ),
    "comma-separated"
  )
})

test_that("endpoint-library presets are drug specific and remain editable", {
  phenytoin <- .lator_endpoint_templates_for_gui("phenytoin")
  suggested <- Filter(function(template) isTRUE(template$recommended), phenytoin)
  expect_length(suggested, 1L)
  expect_equal(suggested[[1L]]$id, "template-aed-range")
  defaults <- stats::setNames(
    lapply(suggested[[1L]]$fields, `[[`, "default"),
    vapply(suggested[[1L]]$fields, `[[`, character(1), "name")
  )
  expect_equal(defaults$drug, "phenytoin")
  expect_equal(defaults$lower, 10)
  expect_equal(defaults$upper, 20)
  expect_match(defaults$source, "Reimers")

  warfarin <- .lator_endpoint_templates_for_gui("warfarin")
  suggested <- Filter(function(template) isTRUE(template$recommended), warfarin)
  expect_equal(suggested[[1L]]$id, "template-warfarin")
  defaults <- stats::setNames(
    lapply(suggested[[1L]]$fields, `[[`, "default"),
    vapply(suggested[[1L]]$fields, `[[`, character(1), "name")
  )
  expect_equal(unname(unlist(
    defaults[c("lower", "upper", "target_fraction")]
  )),
               c(2, 3, 0.65))

  unknown <- .lator_endpoint_templates_for_gui("unlisted medicine")
  expect_false(any(vapply(
    unknown, function(template) isTRUE(template$recommended), logical(1)
  )))
})

test_that("therapeutic-class suggestions do not borrow drug-specific targets", {
  templates <- .lator_endpoint_templates_for_gui(
    "unlisted antiseizure medicine", "antiseizure"
  )
  suggested <- Filter(function(template) isTRUE(template$recommended), templates)
  expect_length(suggested, 1L)
  expect_equal(suggested[[1L]]$id, "template-aed-range")
  fields <- stats::setNames(
    suggested[[1L]]$fields,
    vapply(suggested[[1L]]$fields, `[[`, character(1), "name")
  )
  expect_equal(fields$drug$default, "unlisted antiseizure medicine")
  expect_equal(fields$lower$default, "")
  expect_equal(fields$upper$default, "")
})

test_that("endpoint modification payload retains values and advances version", {
  current <- lator_endpoint_aed(
    "phenytoin", 10, 20, "mg/L", "Reviewed local protocol",
    status = "reviewed"
  )
  current$metadata$template_id <- "template-aed-range"
  occupied <- current
  occupied$version <- "1.0.1"
  edit <- .lator_endpoint_edit_payload(
    current,
    list(
      "aed-phenytoin@1.0.0" = current,
      "aed-phenytoin@1.0.1" = occupied
    ),
    original_key = "aed-phenytoin@1.0.0"
  )
  expect_equal(edit$template_id, "template-aed-range")
  expect_equal(edit$values$lower, 10)
  expect_equal(edit$values$upper, 20)
  expect_equal(edit$values$source, "Reviewed local protocol")
  expect_equal(edit$values$version, "1.0.2")
})

test_that("AUC/MIC and composite antimicrobial endpoints evaluate trajectories", {
  patient <- lator_patient_new("ANTIMICROBIAL-001")
  patient <- lator_patient_add_event(patient, "covariate", 0, "MIC", 1, "mg/L")
  predictions <- data.frame(TIME = c(0, 12, 24), IPRED = c(20, 20, 20))
  vancomycin <- lator_endpoint_vancomycin(
    lower = 400, upper = 600, source = "Institutional protocol"
  )
  evaluated <- lator_endpoint_evaluate(vancomycin, predictions, patient)
  expect_equal(evaluated$median_metric, 480)
  expect_equal(evaluated$attainment_probability, 1)

  aminoglycoside <- lator_endpoint_aminoglycoside(
    "Example aminoglycoside", efficacy_lower = 8, trough_upper = 2,
    source = "Institutional protocol"
  )
  amino_predictions <- data.frame(TIME = c(0, 12, 24), IPRED = c(10, 4, 1))
  amino <- lator_endpoint_evaluate(
    aminoglycoside, amino_predictions, patient
  )
  expect_equal(amino$median_metric, 10)
  expect_equal(amino$attainment_probability, 1)
})

test_that("transplant, timed-clearance, and response endpoints are executable", {
  trough <- lator_endpoint_tacrolimus(
    5, 10, "ng/mL", "Institutional protocol"
  )
  expect_equal(lator_endpoint_evaluate(
    trough, data.frame(TIME = 0:2, IPRED = c(9, 7, 6))
  )$attainment_probability, 1)

  mpa <- lator_endpoint_mycophenolate(
    30, 70, "mg*h/L", "Institutional protocol"
  )
  expect_equal(lator_endpoint_evaluate(
    mpa, data.frame(TIME = c(0, 6, 12), IPRED = c(5, 5, 5))
  )$median_metric, 60)

  busulfan <- lator_endpoint_busulfan(
    100, 140, "mg*h/L", "Institutional protocol", doses = 2
  )
  expect_equal(lator_endpoint_evaluate(
    busulfan, data.frame(TIME = c(0, 6, 12), IPRED = c(5, 5, 5))
  )$median_metric, 120)

  methotrexate <- lator_endpoint_methotrexate(
    data.frame(hours_after_dose = c(24, 48), upper = c(10, 1)),
    "micromol/L", "Institutional protocol"
  )
  patient <- lator_patient_new("MTX-001")
  patient <- lator_patient_add_event(
    patient, "dose", 0, "methotrexate", 1000, "mg"
  )
  mtx <- lator_endpoint_evaluate(
    methotrexate,
    data.frame(TIME = c(0, 24, 48), IPRED = c(100, 8, 0.5)),
    patient
  )
  expect_equal(mtx$attainment_probability, 1)

  warfarin <- lator_endpoint_warfarin(
    2, 3, 0.7, "Institutional protocol"
  )
  inr <- lator_endpoint_evaluate(
    warfarin, data.frame(TIME = c(0, 12, 24), IPRED = c(2.2, 2.5, 2.8))
  )
  expect_equal(inr$median_metric, 1)
  expect_equal(inr$attainment_probability, 1)
})
