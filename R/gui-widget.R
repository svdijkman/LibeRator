.lator_gui_rows <- function(values) {
  if (!length(values)) return(list())
  if (is.data.frame(values)) {
    return(unname(lapply(seq_len(nrow(values)), function(index) as.list(values[index, , drop = FALSE]))))
  }
  unname(lapply(values, function(value) as.list(value)))
}

.lator_gui_json_safe <- function(value) {
  if (is.data.frame(value)) {
    list_columns <- vapply(value, is.list, logical(1))
    value[list_columns] <- lapply(
      value[list_columns],
      function(column) lapply(column, .lator_gui_json_safe)
    )
    return(value)
  }
  if (is.atomic(value) && length(value) && !is.null(names(value))) {
    return(stats::setNames(
      lapply(unname(as.list(value)), .lator_gui_json_safe),
      names(value)
    ))
  }
  if (is.list(value)) {
    result <- lapply(value, .lator_gui_json_safe)
    names(result) <- names(value)
    return(result)
  }
  value
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
                               endpoint_prompt = 0L, hosted = FALSE,
                               model_discovery = TRUE,
                               model_discovery_reason = "",
                               model_library = list(),
                               model_library_loaded = FALSE,
                               model_library_available = TRUE,
                               model_library_reason = "") {
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
  event_row <- function(event, active = TRUE, superseded_by = character()) {
    correction <- event$metadata$correction %||% list()
    list(
      id = event$event_id, type = event$type, time = event$time,
      name = event$name,
      value = if (length(event$value) == 1L && !is.na(event$value)) {
        event$value
      } else NULL,
      unit = event$unit, source = event$source,
      missing = event$missing_reason,
      occasion = if (is.finite(event$occasion)) event$occasion else NULL,
      metadata = event$metadata %||% list(),
      recordedAt = event$recorded_at,
      supersedes = as.character(event$supersedes %||% ""),
      supersededBy = as.character(superseded_by),
      active = isTRUE(active),
      status = if (identical(event$type, "correction")) {
        "entered_in_error"
      } else if (!isTRUE(active)) {
        "superseded"
      } else if (length(correction)) {
        "corrected"
      } else "active",
      correctionReason = as.character(correction$reason %||% ""),
      correctionActor = as.character(correction$actor %||% ""),
      amendable = isTRUE(active) && !identical(event$type, "correction")
    )
  }
  events <- if (is.null(patient)) list() else {
    lapply(.lator_active_events(patient), event_row)
  }
  event_ledger <- if (is.null(patient)) list() else {
    all_events <- patient$events
    superseded_ids <- vapply(all_events, function(event) {
      as.character(event$supersedes %||% "")
    }, character(1))
    lapply(all_events, function(event) {
      replacement_ids <- vapply(
        all_events[superseded_ids == event$event_id],
        `[[`, character(1), "event_id"
      )
      event_row(
        event,
        active = !event$event_id %in% superseded_ids[nzchar(superseded_ids)],
        superseded_by = replacement_ids
      )
    })
  }
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
    profile_data <- as.data.frame(
      item$individual_profile %||% data.frame()
    )
    legacy_profile <- !nrow(profile_data) ||
      !all(c("TIME", "IPRED", "DV") %in% names(profile_data))
    if (legacy_profile) {
      profile_data <- predictions
    }
    profile <- if (nrow(profile_data) &&
                   all(c("TIME", "IPRED") %in% names(profile_data))) {
      lapply(seq_len(nrow(profile_data)), function(index) {
        ipred <- suppressWarnings(as.numeric(profile_data$IPRED[[index]]))
        observation <- if ("DV" %in% names(profile_data)) {
          suppressWarnings(as.numeric(profile_data$DV[[index]]))
        } else NA_real_
        population_value <- function(name) {
          if (!name %in% names(profile_data)) return(NULL)
          result <- suppressWarnings(as.numeric(profile_data[[name]][[index]]))
          if (is.finite(result)) result else NULL
        }
        list(
          time = as.numeric(profile_data$TIME[[index]]),
          ipred = if (is.finite(ipred)) ipred else NULL,
          observation = if (is.finite(observation)) observation else NULL,
          populationLower = population_value("POP_LOWER"),
          populationMedian = population_value("POP_MEDIAN"),
          populationUpper = population_value("POP_UPPER"),
          observedTime = population_value("OBSERVED_TIME"),
          eventId = if ("EVENT_ID" %in% names(profile_data)) {
            as.character(profile_data$EVENT_ID[[index]])
          } else "",
          kind = if ("kind" %in% names(profile_data)) {
            as.character(profile_data$kind[[index]])
          } else if (is.finite(observation)) "observation" else "curve"
        )
      })
    } else list()
    individual_parameters <- item$individual_parameters %||%
      tryCatch(
        .lator_individual_parameters(item),
        error = function(error) data.frame()
      )
    if (nrow(individual_parameters) &&
        (!"kind" %in% names(individual_parameters) ||
         !"expression" %in% names(individual_parameters) ||
         !"individualised" %in% names(individual_parameters))) {
      model <- item$model %||% NULL
      details <- if (inherits(model, "nm_model")) {
        .lator_model_output_details(model, individual_parameters$parameter)
      } else data.frame()
      individual_parameters$kind <- if (nrow(details)) {
        details$kind[match(individual_parameters$parameter, details$name)]
      } else "parameter"
      individual_parameters$expression <- if (nrow(details)) {
        details$expression[match(individual_parameters$parameter, details$name)]
      } else ""
      individual_parameters$individualised <- if (nrow(details)) {
        details$individualised[
          match(individual_parameters$parameter, details$name)
        ]
      } else FALSE
    }
    list(
      id = item$assessment_id, at = item$created_at, cutoff = item$cutoff,
      mode = item$mode, convergence = item$convergence,
      eta = .lator_gui_rows(item$eta_trajectory),
      individualParameters = .lator_gui_rows(individual_parameters),
      profile = profile,
      profileInterval = if (legacy_profile) list(
        profile_type = "legacy_event_only",
        source = "legacy event-time predictions"
      ) else item$individual_profile_interval %||% NULL,
      target = item$endpoint_evaluation, diagnostics = item$diagnostics,
      warnings = unique(c(
        as.character(item$warnings %||% character()),
        if (legacy_profile) paste(
          "This saved assessment predates dense profile prediction.",
          "Run a new individual assessment to calculate the full",
          "post-dose curve and similar-patient prediction interval."
        )
      ))
    )
  })
  current <- if (length(assessments)) utils::tail(assessments, 1L)[[1L]] else NULL
  current_item <- if (length(patient_assessments)) {
    utils::tail(patient_assessments, 1L)[[1L]]
  } else NULL
  readiness_changes <- list(
    patient = FALSE, medication = FALSE, endpoint = FALSE,
    model = FALSE, tdm = FALSE
  )
  changes_known <- FALSE
  if (!is.null(patient) && !is.null(current_item) &&
      nrow(selected_medication) == 1L) {
    hashes <- .lator_assessment_input_hashes(
      patient, selected_medication$drug[[1L]]
    )
    changes_known <- all(c(
      "medication_hash", "tdm_hash", "patient_context_hash"
    ) %in% names(current_item))
    if (changes_known) {
      readiness_changes$patient <- !identical(
        current_item$patient_context_hash, hashes$patient_context_hash
      )
      readiness_changes$medication <- !identical(
        current_item$medication_hash, hashes$medication_hash
      )
      readiness_changes$tdm <- !identical(
        current_item$tdm_hash, hashes$tdm_hash
      )
    } else if (nzchar(as.character(
      current_item$evidence_hash %||% ""
    ))) {
      readiness_changes$patient <- !identical(
        current_item$evidence_hash,
        .lator_hash(.lator_active_events(patient))
      )
    }
    current_endpoint <- if (
      !is.null(selected_endpoint) && length(selected_endpoint) &&
        selected_endpoint %in% names(endpoints)
    ) endpoints[[selected_endpoint]] else NULL
    current_model <- if (
      !is.null(selected_model) && length(selected_model) &&
        selected_model %in% names(models)
    ) models[[selected_model]] else NULL
    readiness_changes$endpoint <- !is.null(current_endpoint) &&
      !identical(
        current_item$endpoint_hash, .lator_hash(current_endpoint)
      )
    readiness_changes$model <- !is.null(current_model) &&
      !identical(current_item$model_hash, .lator_hash(current_model))
  }
  concentration_events <- if (is.null(patient) || !nrow(selected_medication)) {
    list()
  } else {
    .lator_match_therapy_events(
      patient, "concentration", selected_medication$drug[[1L]]
    )
  }
  measured_concentrations <- Filter(function(event) {
    result <- suppressWarnings(as.numeric(event$value))
    length(result) == 1L && is.finite(result)
  }, concentration_events)
  dynamic_status <- if (
    !is.null(patient) && nrow(selected_medication) == 1L
  ) {
    .lator_dynamic_evidence_status(
      patient, selected_medication$drug[[1L]]
    )
  } else list(
    ready = FALSE, state_count = 0L, observed_state_count = 0L,
    boundary_count = 0L,
    reason = "Select a patient and medication first."
  )
  profile_observation <- if (
    !is.null(patient) && nrow(selected_medication) == 1L &&
      length(measured_concentrations)
  ) {
    automatic <- .lator_profile_observation_selection(
      patient, selected_medication$drug[[1L]], scope = "automatic"
    )
    list(
      totalCount = automatic$total_count,
      automaticCount = automatic$selected_count,
      automaticLabel = automatic$label,
      episodeStart = automatic$episode_start,
      episodeGapHours = automatic$episode_gap_hours
    )
  } else list(
    totalCount = 0L, automaticCount = 0L,
    automaticLabel = "No measured TDM", episodeStart = NULL,
    episodeGapHours = 1008
  )
  visible_model_ids <- names(models)
  if (nrow(selected_medication)) {
    active_drug <- .lator_canonical_drug(selected_medication$drug[[1L]])
    visible_model_ids <- visible_model_ids[vapply(
      models[visible_model_ids],
      function(model) {
        scope <- as.character(
          attr(model, "lator_medications", exact = TRUE) %||% character()
        )
        !length(scope) || active_drug %in%
          vapply(scope, .lator_canonical_drug, character(1))
      },
      logical(1)
    )]
  }
  model_rows <- lapply(visible_model_ids, function(id) list(
    id = id, name = attr(models[[id]], "name", exact = TRUE) %||% id,
    advan = models[[id]]$ADVAN, trans = models[[id]]$TRANS,
    etas = models[[id]]$n_eta, source = if (length(attr(models[[id]], "library_provenance", exact = TRUE))) "LibeRary" else "local"
  ))
  selected_model_info <- if (
    !is.null(selected_model) && length(selected_model) &&
      selected_model %in% names(models)
  ) {
    tryCatch(
      .lator_model_info_for_gui(models[[selected_model]], selected_model),
      error = function(error) list(
        id = selected_model,
        name = attr(models[[selected_model]], "name", exact = TRUE) %||%
          selected_model,
        source = "Local/LibeRation model",
        structure = paste0(
          "ADVAN", models[[selected_model]]$ADVAN,
          "/TRANS", models[[selected_model]]$TRANS
        ),
        parameters = list(), covariates = list(), derived = list(),
        limitations = paste(
          "Detailed model metadata could not be generated:",
          conditionMessage(error)
        )
      )
    )
  } else NULL
  endpoint_key_for <- function(endpoint) {
    hashes <- vapply(endpoints, .lator_hash, character(1))
    matched <- names(hashes)[hashes == .lator_hash(endpoint)]
    if (length(matched)) matched[[1L]] else ""
  }
  endpoint_components_for <- function(endpoint) {
    if (!identical(endpoint$kind, "multi_endpoint")) return(list())
    lapply(endpoint$rules$components, function(component) list(
      componentId = component$component_id,
      endpointKey = endpoint_key_for(component$endpoint),
      endpointId = component$endpoint$id,
      endpointVersion = component$endpoint$version,
      name = component$endpoint$name,
      role = component$role,
      weight = component$weight,
      hardConstraint = component$hard_constraint,
      minimumAttainment = if (component$hard_constraint) {
        component$minimum_attainment
      } else NULL
    ))
  }
  endpoint_rows <- lapply(names(endpoints), function(id) {
    endpoint <- endpoints[[id]]
    target <- .lator_target_range(endpoint)
    list(
      id = id, name = endpoint$name, drug = endpoint$drug,
      drugKey = .lator_drug_key(endpoint$drug),
      kind = endpoint$kind, status = endpoint$status,
      version = endpoint$version,
      isSet = identical(endpoint$kind, "multi_endpoint"),
      components = endpoint_components_for(endpoint),
      lower = target$lower %||% endpoint$rules$lower %||% NULL,
      upper = target$upper %||% endpoint$rules$upper %||% NULL,
      unit = target$unit %||% endpoint$unit,
      source = endpoint$source
    )
  })
  selected_endpoint_definition <- if (
    !is.null(selected_endpoint) && length(selected_endpoint) &&
      selected_endpoint %in% names(endpoints)
  ) endpoints[[selected_endpoint]] else NULL
  regimen_defaults <- selected_endpoint_definition$metadata$regimen_grid %||%
    list(
      amounts = c(100, 200, 300), intervals = c(12, 24),
      horizon = 168, posterior_draws = 100L
    )
  endpoint_edit <- if (
    !is.null(selected_endpoint) && length(selected_endpoint) &&
      selected_endpoint %in% names(endpoints)
  ) {
    selected_definition <- endpoints[[selected_endpoint]]
    if (identical(selected_definition$kind, "multi_endpoint")) {
      list(
        kind = "multi_endpoint",
        original_key = selected_endpoint,
        original_version = selected_definition$version,
        name = selected_definition$name,
        source = selected_definition$source,
        status = selected_definition$status,
        version = .lator_next_endpoint_version(
          selected_definition, endpoints
        ),
        components = endpoint_components_for(selected_definition)
      )
    } else {
      .lator_endpoint_edit_payload(
        selected_definition, endpoints,
        original_key = selected_endpoint
      )
    }
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
  prediction_rows <- lapply(prediction_items, function(item) {
    endpoint_outcomes <- item$endpoint_outcomes %||% tryCatch(
      .lator_endpoint_outcome_summary(
        item$endpoint, item$evaluation,
        probs = item$interval_probabilities %||% c(0.05, 0.5, 0.95)
      ),
      error = function(error) data.frame()
    )
    list(
      id = item$prediction_id, candidateId = item$candidate_id,
      assessmentId = item$assessment_id,
      regimen = as.list(item$regimen[1L, , drop = FALSE]),
      forecast = .lator_gui_rows(item$forecast),
      endpointOutcomes = .lator_gui_rows(endpoint_outcomes),
      steadyStateForecast = .lator_gui_rows(
        item$steady_state_forecast %||% data.frame()
      ),
      steadyState = if (is.null(item$steady_state)) NULL else list(
        profileType = item$steady_state$profile_type,
        summary = item$steady_state$summary,
        evaluation = item$steady_state$evaluation,
        horizonConvergence = item$steady_state$horizon_convergence
      ),
      target = item$target,
      evaluation = item$evaluation,
      transitionEvaluation = item$transition_evaluation,
      probabilities = as.numeric(item$interval_probabilities),
      generatedAt = item$generated_at
    )
  })
  list(
    patients = .lator_gui_rows(patients), patient = if (is.null(patient)) NULL else list(
      id = patient$patient_id, label = patient$label, study = patient$study_id,
      revision = patient$revision, eventCount = length(patient$events), status = patient$status
    ),
    events = events, eventLedger = event_ledger,
    assessments = assessments, current = current,
    medications = .lator_gui_rows(medications),
    selectedDrug = selected_drug,
    models = model_rows, endpoints = endpoint_rows,
    selectedModelInfo = selected_model_info,
    selectedModel = selected_model, selectedEndpoint = selected_endpoint,
    regimen = if (is.null(regimen)) NULL else list(
      summary = .lator_gui_rows(regimen$summary),
      endpointKind = regimen$endpoint$kind,
      endpointName = regimen$endpoint$name,
      componentResults = if (
        identical(regimen$endpoint$kind, "multi_endpoint")
      ) {
        Filter(Negate(is.null), lapply(regimen$trajectories, function(item) {
          if (is.null(item) || is.null(item$evaluation$components)) {
            return(NULL)
          }
          list(
            candidateId = as.character(
              item$candidate$candidate_id[[1L]]
            ),
            components = .lator_gui_rows(item$evaluation$components)
          )
        }))
      } else list(),
      selectedCandidates = as.character(selected_candidate %||% character())
    ),
    predictions = unname(prediction_rows),
    profileObservation = profile_observation,
    regimenDefaults = list(
      amounts = as.numeric(regimen_defaults$amounts),
      intervals = as.numeric(regimen_defaults$intervals),
      horizon = as.numeric(regimen_defaults$horizon %||% 168),
      posteriorDraws = as.integer(
        regimen_defaults$posterior_draws %||% 100L
      )
    ),
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
    libraryModels = unname(model_library),
    modelLibraryLoaded = isTRUE(model_library_loaded),
    modelLibraryAvailable = isTRUE(model_library_available),
    modelLibraryReason = as.character(model_library_reason %||% "")[[1L]],
    modelTemplates = .lator_model_templates_for_gui(),
    readiness = list(
      patient = !is.null(patient),
      medication = nrow(selected_medication) == 1L,
      endpoint = !is.null(selected_endpoint) && nzchar(selected_endpoint),
      model = !is.null(selected_model) && nzchar(selected_model),
      tdm = length(measured_concentrations) > 0L,
      assessment = !is.null(current),
      assessmentReady = !is.null(patient) && nrow(selected_medication) == 1L &&
        !is.null(selected_endpoint) && nzchar(selected_endpoint) &&
        !is.null(selected_model) && nzchar(selected_model) &&
        length(measured_concentrations) > 0L,
      concentrationCount = length(measured_concentrations),
      dynamicReady = isTRUE(dynamic_status$ready),
      dynamicStateCount = dynamic_status$state_count,
      dynamicObservedStateCount = dynamic_status$observed_state_count,
      dynamicReason = dynamic_status$reason,
      changed = readiness_changes,
      changesKnown = changes_known,
      anyChanged = any(unlist(readiness_changes, use.names = FALSE))
    ),
    hosted = isTRUE(hosted),
    modelDiscoveryAvailable = isTRUE(model_discovery),
    modelDiscoveryReason = as.character(model_discovery_reason %||% "")[[1L]],
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
  payload <- .lator_gui_json_safe(c(payload, list(inputId = input_id)))
  content <- reactR::component("LibeRatorWorkbench", payload)
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
