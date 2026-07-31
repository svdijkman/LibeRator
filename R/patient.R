.lator_empty_catalog <- function() list(schema = "liberator.catalog", version = 1L, patients = list())

.lator_catalog_read <- function(workspace) {
  .lator_encrypt_read(workspace$paths$catalog, workspace$key, .lator_empty_catalog())
}

.lator_patient_path <- function(workspace, patient_id) {
  file.path(workspace$paths$records, paste0(.lator_record_token(patient_id, workspace$key), ".enc"))
}

.lator_validate_patient <- function(patient) {
  if (!inherits(patient, "lator_patient") || !identical(patient$schema, "liberator.patient") ||
      as.integer(patient$version) != 1L) .lator_stop("Invalid LibeRator patient record.")
  patient$patient_id <- .lator_scalar(patient$patient_id, "patient_id", max_chars = 128L)
  patient$model_selections <- patient$model_selections %||% list()
  patient$therapies <- patient$therapies %||% list()
  if (!is.list(patient$events) || !is.list(patient$assessments) ||
      !is.list(patient$therapies) || !is.list(patient$model_selections)) {
    .lator_stop("Patient timelines, assessments, therapies, and model selections must be lists.")
  }
  ids <- vapply(patient$events, function(event) as.character(event$event_id %||% ""), character(1))
  if (any(!nzchar(ids)) || anyDuplicated(ids)) .lator_stop("Patient event ids must be unique.")
  patient
}

#' Create a pseudonymous longitudinal patient record
#'
#' Direct identifiers such as name, address, date of birth, hospital number,
#' email, or telephone number are intentionally absent. Linkage to an
#' identifiable clinical record belongs in the controlling institution's
#' separately governed system.
#'
#' @param patient_id Pseudonym assigned outside LibeRator.
#' @param study_id Optional research-study identifier.
#' @param label Non-identifying display label.
#' @param metadata Non-identifying study metadata.
#' @return A `lator_patient`.
#' @export
lator_patient_new <- function(patient_id, study_id = "", label = "", metadata = list()) {
  patient_id <- .lator_scalar(patient_id, "patient_id", max_chars = 128L)
  study_id <- .lator_scalar(study_id, "study_id", allow_empty = TRUE, max_chars = 128L)
  label <- .lator_scalar(label, "label", allow_empty = TRUE, max_chars = 128L)
  if (!is.list(metadata) || is.null(names(metadata)) && length(metadata)) {
    .lator_stop("`metadata` must be a named list.")
  }
  now <- .lator_now()
  structure(list(
    schema = "liberator.patient", version = 1L, revision = 0L,
    patient_id = patient_id, study_id = study_id, label = label,
    metadata = metadata, created_at = now, updated_at = now,
    events = list(), therapies = list(), assessments = list(),
    model_selections = list(),
    status = "active"
  ), class = "lator_patient")
}

#' @export
print.lator_patient <- function(x, ...) {
  cat("LibeRator longitudinal patient\n")
  cat("  pseudonym:", x$patient_id, " revision:", x$revision, "\n")
  cat("  events:", length(x$events), " assessments:", length(x$assessments),
      " therapies:", length(x$therapies %||% list()),
      " model selections:", length(x$model_selections %||% list()), "\n")
  invisible(x)
}

.lator_drug_key <- function(drug) {
  drug <- tolower(.lator_scalar(drug, "drug", max_chars = 128L))
  key <- gsub("[^a-z0-9]+", "-", drug)
  key <- gsub("(^-+|-+$)", "", key)
  if (!nzchar(key)) .lator_stop("The drug name cannot form a stable key.")
  key
}

.lator_patient_endpoint_instance_key <- function(patient, drug, endpoint) {
  patient <- .lator_validate_patient(patient)
  endpoint <- lator_endpoint_validate(endpoint)
  paste0(
    "patient-", substr(.lator_hash(patient$patient_id), 1L, 12L),
    "-", .lator_drug_key(drug), "-", endpoint$id, "@", endpoint$version
  )
}

.lator_empty_medications <- function() data.frame(
  key = character(), drug = character(), therapeutic_class = character(),
  monitoring_analytes = character(),
  last_dose_time = numeric(), endpoint_key = character(),
  model_id = character(),
  treatment_status = character(),
  stringsAsFactors = FALSE
)

#' Add a medication to a patient treatment profile
#'
#' A medication can be represented before its first dose record is available.
#' Dose and TDM evidence remain separate immutable timeline events.
#'
#' @param patient Patient record.
#' @param drug Medication name.
#' @param therapeutic_class Optional medication-class label used for endpoint
#'   family suggestions.
#' @param monitoring_analytes Optional drug metabolites or related analytes
#'   that may be selected when recording TDM evidence.
#' @return Updated patient record. Persist it with [lator_patient_save()].
#' @export
lator_patient_medication_add <- function(
    patient, drug, therapeutic_class = "",
    monitoring_analytes = character()) {
  patient <- .lator_validate_patient(patient)
  drug <- .lator_scalar(drug, "drug", max_chars = 128L)
  therapeutic_class <- .lator_scalar(
    therapeutic_class, "therapeutic_class", allow_empty = TRUE,
    max_chars = 128L
  )
  monitoring_analytes <- unique(trimws(as.character(
    monitoring_analytes %||% character()
  )))
  monitoring_analytes <- monitoring_analytes[
    !is.na(monitoring_analytes) & nzchar(monitoring_analytes)
  ]
  if (any(nchar(monitoring_analytes) > 128L)) {
    .lator_stop("Monitoring analyte names may contain at most 128 characters.")
  }
  key <- .lator_drug_key(drug)
  if (!is.null(patient$therapies[[key]])) {
    .lator_stop("Medication `", drug, "` is already in this patient profile.")
  }
  now <- .lator_now()
  patient$therapies[[key]] <- list(
    schema = "liberator.therapy_profile", schema_version = 1L,
    drug = drug, therapeutic_class = therapeutic_class,
    monitoring_analytes = monitoring_analytes,
    treatment_status = "active", added_at = now,
    endpoint_key = "", endpoint_id = "", endpoint_version = "",
    endpoint_hash = "", selected_at = "",
    model_id = "", model_hash = "", model_selected_at = "",
    endpoint_history = list()
  )
  patient$updated_at <- now
  patient
}

#' List medications represented on a patient timeline
#'
#' The list combines explicit treatment profiles with active dose evidence and
#' retains any endpoint preference already recorded for each medication. A
#' dose-discontinuation workflow can later supersede or qualify this evidence;
#' absence of a recent dose is not silently interpreted as discontinuation.
#'
#' @param patient Patient record.
#' @param cutoff Latest patient-timeline time to inspect.
#' @return A data frame with one row per recorded medication.
#' @export
lator_patient_medications <- function(patient, cutoff = Inf) {
  patient <- .lator_validate_patient(patient)
  cutoff <- .lator_number(cutoff, "cutoff", finite = FALSE)
  doses <- .lator_active_events(patient, types = "dose", cutoff = cutoff)
  dose_keys <- if (length(doses)) {
    vapply(doses, function(event) .lator_drug_key(
      event$metadata$drug %||% event$name
    ), character(1))
  } else character()
  groups <- if (length(doses)) split(doses, dose_keys) else list()
  keys <- unique(c(names(groups), names(patient$therapies)))
  if (!length(keys)) return(.lator_empty_medications())
  rows <- lapply(keys, function(key) {
    events <- groups[[key]] %||% list()
    latest <- if (length(events)) {
      events[[which.max(vapply(events, `[[`, numeric(1), "time"))]]
    } else NULL
    profile <- patient$therapies[[key]] %||% list()
    drug <- if (!is.null(latest)) {
      latest$metadata$drug %||% latest$name
    } else profile$drug
    event_class <- if (!is.null(latest)) {
      as.character(latest$metadata$therapeutic_class %||% "")
    } else ""
    data.frame(
      key = key,
      drug = as.character(drug),
      therapeutic_class = as.character(
        if (nzchar(event_class)) event_class else
          profile$therapeutic_class %||% ""
      ),
      monitoring_analytes = paste(
        unique(as.character(profile$monitoring_analytes %||% character())),
        collapse = "|"
      ),
      last_dose_time = if (is.null(latest)) NA_real_ else as.numeric(latest$time),
      endpoint_key = as.character(profile$endpoint_key %||% ""),
      model_id = as.character(profile$model_id %||% ""),
      treatment_status = as.character(
        profile$treatment_status %||% "active"
      ),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  output <- output[
    order(is.na(output$last_dose_time), -output$last_dose_time, output$drug),
    , drop = FALSE
  ]
  rownames(output) <- NULL
  output
}

#' Record a medication-specific endpoint preference
#'
#' @param patient Patient record.
#' @param drug Medication name represented by dose evidence.
#' @param endpoint_key Stable patient-assignment key. GUI assignments use a
#'   patient-specific key derived from the endpoint identity and version.
#' @param endpoint Endpoint definition.
#' @param therapeutic_class Optional medication-class label.
#' @return Updated patient record. Persist it with [lator_patient_save()].
#' @export
lator_patient_endpoint_set <- function(
    patient, drug, endpoint_key, endpoint, therapeutic_class = "") {
  patient <- .lator_validate_patient(patient)
  drug <- .lator_scalar(drug, "drug", max_chars = 128L)
  endpoint_key <- .lator_scalar(
    endpoint_key, "endpoint_key", max_chars = 256L
  )
  endpoint <- lator_endpoint_validate(endpoint)
  if (!identical(.lator_drug_key(endpoint$drug), .lator_drug_key(drug))) {
    .lator_stop(
      "Endpoint drug `", endpoint$drug,
      "` does not match selected medication `", drug, "`."
    )
  }
  key <- .lator_drug_key(drug)
  existing <- patient$therapies[[key]] %||% list()
  history <- existing$endpoint_history %||% list()
  if (!length(history) && nzchar(as.character(
    existing$endpoint_key %||% ""
  ))) {
    history[[1L]] <- existing[c(
      "endpoint_key", "endpoint_id", "endpoint_version", "endpoint_hash",
      "selected_at"
    )]
  }
  selection <- list(
    endpoint_key = endpoint_key, endpoint_id = endpoint$id,
    endpoint_version = endpoint$version,
    endpoint_hash = .lator_hash(endpoint), selected_at = .lator_now(),
    endpoint_snapshot = endpoint
  )
  last <- if (length(history)) history[[length(history)]] else NULL
  if (is.null(last) ||
      !identical(last$endpoint_key, selection$endpoint_key) ||
      !identical(last$endpoint_hash, selection$endpoint_hash)) {
    history[[length(history) + 1L]] <- selection
  }
  patient$therapies[[key]] <- list(
    schema = "liberator.therapy_profile", schema_version = 1L,
    drug = drug, therapeutic_class = .lator_scalar(
      therapeutic_class, "therapeutic_class", allow_empty = TRUE,
      max_chars = 128L
    ),
    treatment_status = existing$treatment_status %||% "active",
    added_at = existing$added_at %||% .lator_now(),
    monitoring_analytes = existing$monitoring_analytes %||% character(),
    endpoint_key = selection$endpoint_key,
    endpoint_id = selection$endpoint_id,
    endpoint_version = selection$endpoint_version,
    endpoint_hash = selection$endpoint_hash,
    selected_at = selection$selected_at,
    endpoint_snapshot = endpoint,
    model_id = existing$model_id %||% "",
    model_hash = existing$model_hash %||% "",
    model_selected_at = existing$model_selected_at %||% "",
    endpoint_history = history
  )
  patient$updated_at <- .lator_now()
  patient
}

#' Retrieve a medication-specific endpoint preference
#' @param patient Patient record.
#' @param drug Medication name.
#' @return A therapy-profile list, or `NULL`.
#' @export
lator_patient_endpoint_get <- function(patient, drug) {
  patient <- .lator_validate_patient(patient)
  patient$therapies[[.lator_drug_key(drug)]] %||% NULL
}

.lator_patient_model_set <- function(patient, drug, model_id,
                                     model_hash = "") {
  patient <- .lator_validate_patient(patient)
  key <- .lator_drug_key(drug)
  profile <- patient$therapies[[key]] %||% NULL
  if (is.null(profile)) {
    .lator_stop(
      "Medication `", drug,
      "` must be added before selecting a population model."
    )
  }
  model_id <- .lator_scalar(
    model_id, "model_id", allow_empty = TRUE, max_chars = 128L
  )
  model_hash <- .lator_scalar(
    model_hash, "model_hash", allow_empty = TRUE, max_chars = 256L
  )
  profile$model_id <- model_id
  profile$model_hash <- model_hash
  profile$model_selected_at <- if (nzchar(model_id)) .lator_now() else ""
  patient$therapies[[key]] <- profile
  patient$updated_at <- .lator_now()
  patient
}

.lator_patient_model_get <- function(patient, drug) {
  patient <- .lator_validate_patient(patient)
  profile <- patient$therapies[[.lator_drug_key(drug)]] %||% NULL
  if (is.null(profile)) return(NULL)
  list(
    model_id = as.character(profile$model_id %||% ""),
    model_hash = as.character(profile$model_hash %||% ""),
    selected_at = as.character(profile$model_selected_at %||% "")
  )
}

.lator_event_time <- function(time) {
  if (inherits(time, "POSIXt")) return(as.numeric(time) / 3600)
  .lator_number(time, "time")
}

.lator_event_validate <- function(event) {
  allowed <- c("dose", "concentration", "covariate", "biomarker", "outcome",
               "procedure", "state_boundary", "adherence", "note", "correction")
  event$type <- match.arg(as.character(event$type), allowed)
  event$time <- .lator_event_time(event$time)
  event$name <- .lator_scalar(
    event$name, "name",
    allow_empty = event$type %in% c("note", "state_boundary", "correction")
  )
  event$unit <- .lator_scalar(event$unit, "unit", allow_empty = TRUE, max_chars = 64L)
  event$source <- .lator_scalar(event$source, "source", allow_empty = TRUE, max_chars = 128L)
  event$missing_reason <- .lator_scalar(
    event$missing_reason, "missing_reason", allow_empty = TRUE, max_chars = 256L
  )
  if (event$type %in% c("dose", "concentration", "covariate", "biomarker") &&
      (is.null(event$value) || length(event$value) != 1L)) {
    .lator_stop("This event type requires one `value`.")
  }
  if (event$type == "dose" && (!is.numeric(event$value) || is.na(event$value) || event$value <= 0)) {
    .lator_stop("Dose values must be positive numbers.")
  }
  if (event$type %in% c("covariate", "concentration", "biomarker") &&
      length(event$value) == 1L && is.na(event$value) && !nzchar(event$missing_reason)) {
    .lator_stop("Missing measurements require an explicit `missing_reason`.")
  }
  if (!is.list(event$metadata)) .lator_stop("Event metadata must be a list.")
  event
}

#' Add an immutable event to a patient timeline
#'
#' Corrections are represented by a new event whose `supersedes` field points
#' to the earlier event. Existing evidence is never silently overwritten.
#'
#' @param patient Patient record.
#' @param type Event type.
#' @param time Numeric hours on the patient's study timeline, or a POSIX time.
#' @param name Clinical/model variable name.
#' @param value Scalar value.
#' @param unit Unit string.
#' @param source Provenance label.
#' @param missing_reason Reason a scheduled value is unavailable.
#' @param occasion Optional dynamic-parameter epoch.
#' @param supersedes Earlier event id corrected by this event.
#' @param metadata Additional typed event fields such as route, duration, CMT,
#'   LLOQ, assay, pathogen, MIC method, or adherence certainty.
#' @return Updated patient record.
#' @export
lator_patient_add_event <- function(patient, type, time, name = "", value = NA,
                                    unit = "", source = "manual",
                                    missing_reason = "", occasion = NA_integer_,
                                    supersedes = "", metadata = list()) {
  patient <- .lator_validate_patient(patient)
  event <- .lator_event_validate(list(
    event_id = .lator_id("event"), type = type, time = time, name = name,
    value = value, unit = unit, source = source, missing_reason = missing_reason,
    occasion = suppressWarnings(as.integer(occasion)),
    supersedes = as.character(supersedes %||% ""), metadata = metadata,
    recorded_at = .lator_now()
  ))
  if (nzchar(event$supersedes) && !event$supersedes %in%
      vapply(patient$events, `[[`, character(1), "event_id")) {
    .lator_stop("`supersedes` does not identify an existing patient event.")
  }
  patient$events <- c(patient$events, list(event))
  order_index <- order(
    vapply(patient$events, `[[`, numeric(1), "time"),
    vapply(patient$events, `[[`, character(1), "recorded_at")
  )
  patient$events <- patient$events[order_index]
  patient$updated_at <- .lator_now()
  patient
}

#' Correct or withdraw immutable patient evidence
#'
#' The original event is retained. A replacement event is appended with a
#' `supersedes` link, correction reason, actor, original-event hash, and stable
#' correction-chain root. Marking an event as entered in error appends a typed
#' correction tombstone, so the original is excluded from modelling without
#' inventing replacement clinical evidence.
#'
#' @param patient Patient record.
#' @param event_id Active event to correct.
#' @param reason Required reason for the correction.
#' @param replacement Named list of replacement event fields. Unspecified
#'   fields retain their value from the active event. Supported fields are
#'   `type`, `time`, `name`, `value`, `unit`, `source`, `missing_reason`,
#'   `occasion`, and `metadata`.
#' @param entered_in_error If `TRUE`, withdraw the event without replacing it
#'   with usable evidence.
#' @param actor Clinician or system actor responsible for the correction.
#' @return Updated patient record containing both the original and correction.
#' @export
lator_patient_correct_event <- function(
    patient, event_id, reason, replacement = list(),
    entered_in_error = FALSE, actor = "local-clinician") {
  patient <- .lator_validate_patient(patient)
  event_id <- .lator_scalar(event_id, "event_id", max_chars = 128L)
  reason <- .lator_scalar(reason, "reason", max_chars = 1000L)
  actor <- .lator_scalar(actor, "actor", max_chars = 128L)
  if (!is.list(replacement) ||
      (length(replacement) && is.null(names(replacement)))) {
    .lator_stop("`replacement` must be a named list.")
  }
  supported <- c(
    "type", "time", "name", "value", "unit", "source",
    "missing_reason", "occasion", "metadata"
  )
  unknown <- setdiff(names(replacement), supported)
  if (length(unknown)) {
    .lator_stop(
      "Unknown replacement field(s): ", paste(unknown, collapse = ", "), "."
    )
  }
  ids <- vapply(patient$events, `[[`, character(1), "event_id")
  location <- match(event_id, ids)
  if (is.na(location)) {
    .lator_stop("`event_id` does not identify an existing patient event.")
  }
  superseded <- vapply(patient$events, function(event) {
    identical(as.character(event$supersedes %||% ""), event_id)
  }, logical(1))
  if (any(superseded)) {
    .lator_stop(
      "Only active evidence can be corrected. Amend its current replacement ",
      "instead."
    )
  }
  original <- patient$events[[location]]
  if (identical(original$type, "correction")) {
    .lator_stop(
      "An entered-in-error marker cannot itself be amended; add a new ",
      "evidence event if information later becomes available."
    )
  }
  previous_correction <- original$metadata$correction %||% list()
  root_id <- as.character(
    previous_correction$root_event_id %||% original$event_id
  )
  correction <- list(
    action = if (isTRUE(entered_in_error)) {
      "entered_in_error"
    } else "replacement",
    reason = reason, actor = actor,
    corrected_event_id = original$event_id,
    root_event_id = root_id,
    original_event_hash = .lator_hash(original),
    recorded_at = .lator_now()
  )
  metadata <- original$metadata %||% list()
  if (!is.null(replacement$metadata)) {
    if (!is.list(replacement$metadata)) {
      .lator_stop("`replacement$metadata` must be a list.")
    }
    metadata <- utils::modifyList(metadata, replacement$metadata)
  }
  metadata$correction <- correction

  if (isTRUE(entered_in_error)) {
    return(lator_patient_add_event(
      patient, type = "correction", time = original$time,
      name = original$name, value = NA, unit = original$unit,
      source = actor, missing_reason = "", occasion = original$occasion,
      supersedes = original$event_id, metadata = metadata
    ))
  }
  replacement_event <- original[c(
    "type", "time", "name", "value", "unit", "source",
    "missing_reason", "occasion"
  )]
  for (field in intersect(names(replacement), names(replacement_event))) {
    replacement_event[[field]] <- replacement[[field]]
  }
  do.call(lator_patient_add_event, c(
    list(patient = patient),
    replacement_event,
    list(
      supersedes = original$event_id,
      metadata = metadata
    )
  ))
}

#' Add several event records
#' @param patient Patient record.
#' @param events Data frame or list of named event argument lists.
#' @return Updated patient record.
#' @export
lator_patient_add_events <- function(patient, events) {
  if (is.data.frame(events)) {
    events <- lapply(seq_len(nrow(events)), function(index) as.list(events[index, , drop = FALSE]))
  }
  if (!is.list(events)) .lator_stop("`events` must be a data frame or list.")
  for (event in events) patient <- do.call(lator_patient_add_event, c(list(patient = patient), event))
  patient
}

#' Persist a patient with optimistic revision checking
#' @param workspace Unlocked workspace.
#' @param patient Patient record.
#' @param expected_revision Expected stored revision; defaults to the record's
#'   current revision. Prevents two sessions overwriting one another.
#' @param actor Audit actor.
#' @return Saved patient with incremented revision.
#' @export
lator_patient_save <- function(workspace, patient,
                               expected_revision = patient$revision, actor = "local-session") {
  workspace <- .lator_require_workspace(workspace)
  patient <- .lator_validate_patient(patient)
  expected_revision <- as.integer(expected_revision)
  .lator_with_lock(workspace, "workspace-write", function() {
    path <- .lator_patient_path(workspace, patient$patient_id)
    stored <- .lator_encrypt_read(path, workspace$key, NULL)
    actual <- if (is.null(stored)) 0L else as.integer(stored$revision)
    if (!identical(actual, expected_revision)) {
      .lator_stop("Patient revision conflict: expected ", expected_revision,
                  " but the encrypted workspace contains ", actual, ".")
    }
    patient$revision <- actual + 1L
    patient$updated_at <- .lator_now()
    .lator_atomic_encrypt_save(patient, path, workspace$key)
    catalog <- .lator_catalog_read(workspace)
    catalog$patients[[patient$patient_id]] <- list(
      patient_id = patient$patient_id, study_id = patient$study_id,
      label = patient$label, status = patient$status,
      revision = patient$revision, updated_at = patient$updated_at
    )
    .lator_atomic_encrypt_save(catalog, workspace$paths$catalog, workspace$key)
    .lator_audit_append(
      workspace, if (is.null(stored)) "patient_created" else "patient_updated",
      "patient", patient$patient_id,
      detail = list(revision = patient$revision, event_count = length(patient$events),
                    assessment_count = length(patient$assessments),
                    therapy_count = length(patient$therapies %||% list()),
                    model_selection_count = length(patient$model_selections %||% list())),
      actor = actor
    )
    patient
  })
}

#' Permanently delete an encrypted patient record
#'
#' The encrypted record and catalogue entry are removed. A minimal deletion
#' event remains in the encrypted audit chain so the destructive action can be
#' accounted for.
#'
#' @param workspace Unlocked workspace.
#' @param patient_id Patient pseudonym.
#' @param confirmation Must be exactly `"YES"`.
#' @param actor Audit actor.
#' @return Invisibly `TRUE`.
#' @export
lator_patient_delete <- function(
    workspace, patient_id, confirmation, actor = "local-session") {
  workspace <- .lator_require_workspace(workspace)
  patient_id <- .lator_scalar(patient_id, "patient_id")
  confirmation <- .lator_scalar(
    confirmation, "confirmation", max_chars = 16L
  )
  if (!identical(confirmation, "YES")) {
    .lator_stop("Patient deletion requires typing YES exactly.")
  }
  .lator_with_lock(workspace, "workspace-write", function() {
    path <- .lator_patient_path(workspace, patient_id)
    if (!file.exists(path)) {
      .lator_stop("Unknown patient pseudonym: ", patient_id)
    }
    if (!isTRUE(unlink(path) == 0L) || file.exists(path)) {
      .lator_stop("Unable to delete the encrypted patient record.")
    }
    catalog <- .lator_catalog_read(workspace)
    catalog$patients[[patient_id]] <- NULL
    .lator_atomic_encrypt_save(catalog, workspace$paths$catalog, workspace$key)
    .lator_audit_append(
      workspace, "patient_deleted", "patient", patient_id,
      detail = list(confirmed = TRUE), actor = actor
    )
    invisible(TRUE)
  })
}

#' Load a patient
#' @param workspace Unlocked workspace.
#' @param patient_id Pseudonym.
#' @export
lator_patient_get <- function(workspace, patient_id) {
  workspace <- .lator_require_workspace(workspace)
  patient_id <- .lator_scalar(patient_id, "patient_id")
  patient <- .lator_encrypt_read(.lator_patient_path(workspace, patient_id), workspace$key, NULL)
  if (is.null(patient)) .lator_stop("Unknown patient pseudonym: ", patient_id)
  .lator_validate_patient(patient)
}

#' List pseudonymous patient records
#' @param workspace Unlocked workspace.
#' @return Patient catalogue data frame.
#' @export
lator_patient_list <- function(workspace) {
  workspace <- .lator_require_workspace(workspace)
  patients <- .lator_catalog_read(workspace)$patients
  if (!length(patients)) return(data.frame(
    patient_id = character(), study_id = character(), label = character(),
    status = character(), revision = integer(), updated_at = character(),
    stringsAsFactors = FALSE
  ))
  out <- do.call(rbind, lapply(patients, function(value) {
    as.data.frame(value, stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out[order(out$updated_at, decreasing = TRUE), , drop = FALSE]
}
