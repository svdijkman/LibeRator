.lator_gui_rows <- function(values) {
  if (!length(values)) return(list())
  if (is.data.frame(values)) {
    return(unname(lapply(seq_len(nrow(values)), function(index) as.list(values[index, , drop = FALSE]))))
  }
  unname(lapply(values, function(value) as.list(value)))
}

.lator_gui_payload <- function(workspace, patient_id = NULL, models = list(),
                               endpoints = list(), selected_model = NULL,
                               selected_endpoint = NULL, regimen = NULL,
                               selected_candidate = NULL, prediction = NULL,
                               status = list(level = "info", text = "Workbench ready"),
                               icon = NULL,
                               task = list(running = FALSE, id = "", label = "",
                                           cancellable = FALSE),
                               model_selection = NULL, selected_drug = NULL,
                               endpoint_prompt = 0L) {
  patients <- lator_patient_list(workspace)
  patient <- if (!is.null(patient_id) && patient_id %in% patients$patient_id) {
    lator_patient_get(workspace, patient_id)
  } else NULL
  medications <- if (is.null(patient)) {
    .lator_empty_medications()
  } else {
    lator_patient_medications(patient)
  }
  selected_medication <- if (
    !is.null(selected_drug) && length(selected_drug) &&
      selected_drug %in% medications$key
  ) medications[medications$key == selected_drug, , drop = FALSE] else {
    medications[FALSE, , drop = FALSE]
  }
  events <- if (is.null(patient)) list() else lapply(.lator_active_events(patient), function(event) list(
    id = event$event_id, type = event$type, time = event$time, name = event$name,
    value = if (length(event$value) == 1L && !is.na(event$value)) event$value else NULL,
    unit = event$unit, source = event$source, missing = event$missing_reason,
    occasion = if (is.finite(event$occasion)) event$occasion else NULL
  ))
  patient_assessments <- if (is.null(patient)) list() else patient$assessments
  if (nrow(selected_medication) && length(patient_assessments)) {
    active_key <- selected_medication$key[[1L]]
    patient_assessments <- Filter(function(item) {
      analyte <- as.character(
        item$analyte %||% item$endpoint$drug %||% ""
      )
      length(analyte) == 1L && !is.na(analyte) && nzchar(trimws(analyte)) &&
        identical(.lator_drug_key(analyte), active_key)
    }, patient_assessments)
  }
  assessments <- lapply(patient_assessments, function(item) {
    predictions <- as.data.frame(item$predictions %||% data.frame())
    profile <- if (nrow(predictions) &&
                   all(c("TIME", "IPRED") %in% names(predictions))) {
      lapply(seq_len(nrow(predictions)), function(index) list(
        time = as.numeric(predictions$TIME[[index]]),
        ipred = as.numeric(predictions$IPRED[[index]]),
        observation = if ("DV" %in% names(predictions) &&
                             is.finite(as.numeric(predictions$DV[[index]]))) {
          as.numeric(predictions$DV[[index]])
        } else NULL
      ))
    } else list()
    individual_parameters <- item$individual_parameters %||%
      tryCatch(
        .lator_individual_parameters(item),
        error = function(error) data.frame()
      )
    list(
      id = item$assessment_id, at = item$created_at, cutoff = item$cutoff,
      mode = item$mode, convergence = item$convergence,
      eta = .lator_gui_rows(item$eta_trajectory),
      individualParameters = .lator_gui_rows(individual_parameters),
      profile = profile,
      target = item$endpoint_evaluation, diagnostics = item$diagnostics,
      warnings = as.character(item$warnings %||% character())
    )
  })
  current <- if (length(assessments)) utils::tail(assessments, 1L)[[1L]] else NULL
  model_rows <- lapply(names(models), function(id) list(
    id = id, name = attr(models[[id]], "name", exact = TRUE) %||% id,
    advan = models[[id]]$ADVAN, trans = models[[id]]$TRANS,
    etas = models[[id]]$n_eta, source = if (length(attr(models[[id]], "library_provenance", exact = TRUE))) "LibeRary" else "local"
  ))
  endpoint_rows <- lapply(names(endpoints), function(id) list(
    id = id, name = endpoints[[id]]$name, drug = endpoints[[id]]$drug,
    drugKey = .lator_drug_key(endpoints[[id]]$drug),
    kind = endpoints[[id]]$kind, status = endpoints[[id]]$status,
    version = endpoints[[id]]$version,
    lower = endpoints[[id]]$rules$lower %||% NULL,
    upper = endpoints[[id]]$rules$upper %||% NULL,
    unit = endpoints[[id]]$unit, source = endpoints[[id]]$source
  ))
  endpoint_edit <- if (
    !is.null(selected_endpoint) && length(selected_endpoint) &&
      selected_endpoint %in% names(endpoints)
  ) {
    .lator_endpoint_edit_payload(
      endpoints[[selected_endpoint]], endpoints,
      original_key = selected_endpoint
    )
  } else NULL
  selection_row <- if (is.null(model_selection)) NULL else list(
    id = model_selection$selection_id,
    status = model_selection$status,
    selectedModelId = model_selection$selected_model_id,
    selectedModelVersion = model_selection$selected_model_version,
    qualificationId = model_selection$selected_qualification_id,
    reasons = as.character(model_selection$reasons %||% character()),
    candidates = lapply(model_selection$candidates, function(candidate) list(
      id = candidate$id, name = candidate$name, eligible = candidate$eligible,
      score = candidate$score,
      qualificationStatus = candidate$qualification_status,
      blockers = as.character(candidate$blockers %||% character()),
      warnings = as.character(candidate$warnings %||% character())
    ))
  )
  prediction_items <- if (is.null(prediction)) {
    list()
  } else if (inherits(prediction, "lator_future_prediction")) {
    stats::setNames(list(prediction), prediction$candidate_id)
  } else prediction
  prediction_rows <- lapply(prediction_items, function(item) list(
    id = item$prediction_id, candidateId = item$candidate_id,
    assessmentId = item$assessment_id,
    regimen = as.list(item$regimen[1L, , drop = FALSE]),
    forecast = .lator_gui_rows(item$forecast),
    target = item$target,
    evaluation = item$evaluation,
    probabilities = as.numeric(item$interval_probabilities),
    generatedAt = item$generated_at
  ))
  list(
    patients = .lator_gui_rows(patients), patient = if (is.null(patient)) NULL else list(
      id = patient$patient_id, label = patient$label, study = patient$study_id,
      revision = patient$revision, eventCount = length(patient$events), status = patient$status
    ),
    events = events, assessments = assessments, current = current,
    medications = .lator_gui_rows(medications),
    selectedDrug = selected_drug,
    models = model_rows, endpoints = endpoint_rows,
    selectedModel = selected_model, selectedEndpoint = selected_endpoint,
    regimen = if (is.null(regimen)) NULL else list(
      summary = .lator_gui_rows(regimen$summary),
      selectedCandidates = as.character(selected_candidate %||% character())
    ),
    predictions = unname(prediction_rows),
    icon = icon, task = task, modelSelection = selection_row,
    endpointTemplates = .lator_endpoint_templates_for_gui(
      drug = if (nrow(selected_medication)) {
        selected_medication$drug[[1L]]
      } else NULL,
      therapeutic_class = if (nrow(selected_medication)) {
        selected_medication$therapeutic_class[[1L]]
      } else ""
    ),
    endpointEdit = endpoint_edit,
    endpointPrompt = as.integer(endpoint_prompt %||% 0L),
    status = status, packageVersion = tryCatch(
      as.character(utils::packageVersion("LibeRator")), error = function(error) "0.1.0"
    ),
    validationStatus = "research"
  )
}

#' LibeRator React workbench widget
#' @param payload Workbench payload generated by the Shiny application.
#' @param input_id Shiny event prefix.
#' @param width,height Widget dimensions.
#' @param elementId Optional widget element id.
#' @export
liberator_workbench <- function(payload, input_id = "liberator_workbench",
                                width = NULL, height = "100vh", elementId = NULL) {
  content <- reactR::component("LibeRatorWorkbench", c(payload, list(inputId = input_id)))
  htmlwidgets::createWidget(
    name = "liberatorWorkbench", reactR::reactMarkup(content), width = width,
    height = height, package = "LibeRator", elementId = elementId
  )
}

#' @noRd
widget_html.liberatorWorkbench <- function(id, style, class, ...) {
  htmltools::attachDependencies(
    htmltools::tags$div(id = id, class = class, style = style),
    list(reactR::html_dependency_corejs(), reactR::html_dependency_react(), reactR::html_dependency_reacttools())
  )
}

#' Shiny output for the LibeRator workbench
#' @param outputId Output id.
#' @param width,height CSS dimensions.
#' @export
liberatorWorkbenchOutput <- function(outputId, width = "100%", height = "100vh") {
  htmlwidgets::shinyWidgetOutput(outputId, "liberatorWorkbench", width, height, package = "LibeRator")
}

#' Render a LibeRator workbench
#' @param expr Widget expression.
#' @param env Evaluation environment.
#' @param quoted Whether expression is quoted.
#' @export
renderLiberatorWorkbench <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, liberatorWorkbenchOutput, env, quoted = TRUE)
}
