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
  if (!is.list(patient$events) || !is.list(patient$assessments) || !is.list(patient$therapies)) {
    .lator_stop("Patient timelines, assessments, and therapies must be lists.")
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
    status = "active"
  ), class = "lator_patient")
}

#' @export
print.lator_patient <- function(x, ...) {
  cat("LibeRator longitudinal patient\n")
  cat("  pseudonym:", x$patient_id, " revision:", x$revision, "\n")
  cat("  events:", length(x$events), " assessments:", length(x$assessments), "\n")
  invisible(x)
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
  event$name <- .lator_scalar(event$name, "name", allow_empty = event$type %in% c("note", "state_boundary"))
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
                    assessment_count = length(patient$assessments)), actor = actor
    )
    patient
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
