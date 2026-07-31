.lator_selection_statuses <- c(
  "selected", "multiple_suitable_models", "no_suitable_model"
)

.lator_liberary_api <- function(name, minimum_version = "0.7.10") {
  if (!requireNamespace("LibeRary", quietly = TRUE)) {
    .lator_stop("Install LibeRary ", minimum_version, " or later.")
  }
  version <- as.character(utils::packageVersion("LibeRary"))
  path <- tryCatch(
    normalizePath(find.package("LibeRary"), winslash = "/", mustWork = FALSE),
    error = function(error) "unknown path"
  )
  if (utils::compareVersion(version, minimum_version) < 0L) {
    .lator_stop(
      "Loaded LibeRary ", version, " from ", path,
      "; LibeRary ", minimum_version,
      " or later is required. Reinstall the current LibeRary source and restart R."
    )
  }
  if (name %in% getNamespaceExports("LibeRary")) {
    return(getExportedValue("LibeRary", name))
  }
  .lator_stop(
    "Loaded LibeRary ", version, " from ", path,
    " but it does not provide `", name,
    "`. Reinstall LibeRary from the current source and restart R to unload the stale namespace."
  )
}

.lator_normalize_terms <- function(value) {
  value <- trimws(tolower(as.character(unlist(value %||% character(), use.names = FALSE))))
  unique(value[nzchar(value) & !is.na(value)])
}

.lator_merge_named <- function(base, update) {
  if (!is.list(update)) .lator_stop("Context overrides must be a named list.")
  for (name in names(update)) {
    if (is.list(base[[name]]) && is.list(update[[name]])) {
      base[[name]] <- .lator_merge_named(base[[name]], update[[name]])
    } else {
      base[[name]] <- update[[name]]
    }
  }
  base
}

.lator_context_events <- function(patient, type, cutoff, drug = NULL) {
  events <- .lator_active_events(patient, types = type, cutoff = cutoff)
  if (is.null(drug) || !length(events)) return(events)
  matched <- Filter(function(event) {
    candidate <- event$metadata$drug %||% event$name %||% ""
    tolower(trimws(as.character(candidate))) == tolower(trimws(drug))
  }, events)
  matched
}

.lator_latest_covariates <- function(patient, cutoff, at) {
  events <- .lator_active_events(patient, types = "covariate", cutoff = cutoff)
  if (!length(events)) return(list())
  grouped <- split(events, tolower(vapply(events, `[[`, character(1), "name")))
  output <- lapply(grouped, function(values) {
    usable <- Filter(function(event) {
      value <- suppressWarnings(as.numeric(event$value))
      length(value) == 1L && is.finite(value)
    }, values)
    if (!length(usable)) return(NULL)
    event <- usable[[which.max(vapply(usable, `[[`, numeric(1), "time"))]]
    list(
      name = event$name,
      value = as.numeric(event$value),
      unit = event$unit %||% "",
      time = event$time,
      age_hours = max(0, at - event$time),
      source = event$source %||% "",
      event_id = event$event_id
    )
  })
  Filter(Negate(is.null), output)
}

#' Build the model-selection context available at a dosing decision
#'
#' Only evidence at or before `cutoff` is used. Each longitudinal covariate
#' retains its source time and age so model qualifications can reject stale or
#' unsupported substitutions.
#'
#' @param patient LibeRator patient timeline.
#' @param endpoint Selected therapeutic endpoint.
#' @param cutoff Latest patient-timeline hour visible to the decision.
#' @param overrides Explicit non-identifying context overrides.
#' @return A serializable `lator_patient_context`.
#' @export
lator_patient_context <- function(patient, endpoint, cutoff = Inf,
                                  overrides = list()) {
  patient <- .lator_validate_patient(patient)
  endpoint <- lator_endpoint_validate(endpoint)
  cutoff <- .lator_number(cutoff, "cutoff", finite = FALSE)
  visible <- .lator_active_events(patient, cutoff = cutoff)
  latest_event <- if (length(visible)) {
    max(vapply(visible, `[[`, numeric(1), "time"))
  } else 0
  at <- if (is.finite(cutoff)) cutoff else latest_event
  doses <- .lator_context_events(patient, "dose", cutoff, endpoint$drug)
  samples <- .lator_context_events(patient, "concentration", cutoff, endpoint$drug)
  metadata <- patient$metadata %||% list()
  context <- list(
    schema = "liberator.patient_context",
    schema_version = 1L,
    patient_id = patient$patient_id,
    patient_revision = patient$revision,
    at = at,
    drug = endpoint$drug,
    analytes = unique(c(
      endpoint$drug,
      as.character(endpoint$metadata$analytes %||% character())
    )),
    indication = as.character(metadata$indication %||% ""),
    routes = unique(vapply(doses, function(event) {
      as.character(event$metadata$route %||% "")
    }, character(1))),
    formulations = unique(vapply(doses, function(event) {
      as.character(event$metadata$formulation %||% "")
    }, character(1))),
    regimens = unique(vapply(doses, function(event) {
      as.character(event$metadata$regimen %||% "")
    }, character(1))),
    endpoint = list(
      id = endpoint$id, kind = endpoint$kind, version = endpoint$version,
      components = if (identical(endpoint$kind, "multi_endpoint")) {
        lapply(endpoint$rules$components, function(component) list(
          id = component$endpoint$id,
          kind = component$endpoint$kind,
          version = component$endpoint$version,
          role = component$role
        ))
      } else list()
    ),
    covariates = .lator_latest_covariates(patient, cutoff, at),
    assays = list(
      matrices = unique(vapply(samples, function(event) {
        as.character(event$metadata$matrix %||% "")
      }, character(1))),
      methods = unique(vapply(samples, function(event) {
        as.character(event$metadata$assay %||% event$metadata$method %||% "")
      }, character(1))),
      units = unique(vapply(samples, function(event) {
        as.character(event$unit %||% "")
      }, character(1)))
    ),
    genotype = metadata$genotype %||% list(),
    created_at = .lator_now()
  )
  for (name in c("routes", "formulations", "regimens")) {
    context[[name]] <- unique(as.character(context[[name]][nzchar(context[[name]])]))
  }
  for (name in c("matrices", "methods", "units")) {
    values <- context$assays[[name]]
    context$assays[[name]] <- unique(as.character(values[nzchar(values)]))
  }
  context <- .lator_merge_named(context, overrides)
  class(context) <- c("lator_patient_context", "list")
  context
}

.lator_candidate_from_clinical_record <- function(record) {
  list(
    id = as.character(record$library_id %||% record$model$library_id %||% ""),
    name = as.character(record$title %||% record$library_id %||% ""),
    model_version = as.character(record$model_version %||%
                                   record$model$version %||% ""),
    model_hash = as.character(record$model$model_sha256 %||% ""),
    source = "LibeRary",
    qualification = record
  )
}

#' Read clinically scoped LibeRary model candidates
#'
#' @param root Optional LibeRary catalogue root.
#' @param status Qualification statuses to retrieve.
#' @return Candidate records suitable for [lator_model_select()].
#' @export
lator_model_candidates_from_liberary <- function(
    root = NULL, status = c("qualified", "candidate")) {
  qualifications <- .lator_liberary_api(
    "library_clinical_qualifications"
  )
  arguments <- list(status = status, current = TRUE)
  if (!is.null(root)) arguments$root <- root
  records <- do.call(qualifications, arguments)
  unname(lapply(records, .lator_candidate_from_clinical_record))
}

.lator_context_covariate <- function(context, name) {
  covariates <- context$covariates %||% list()
  if (!length(covariates)) return(NULL)
  keys <- tolower(names(covariates))
  index <- match(tolower(name), keys)
  if (is.na(index)) {
    labels <- vapply(covariates, function(value) {
      tolower(as.character(value$name %||% ""))
    }, character(1))
    index <- match(tolower(name), labels)
  }
  if (is.na(index)) NULL else covariates[[index]]
}

.lator_review_overdue <- function(value) {
  value <- as.character(value %||% "")[[1L]]
  if (!nzchar(value)) return(FALSE)
  parsed <- suppressWarnings(as.POSIXct(value, tz = "UTC"))
  is.na(parsed) || parsed < Sys.time()
}

.lator_restricted_match <- function(scope, context, field, blockers, warnings,
                                    missing_is_blocker = TRUE) {
  accepted <- .lator_normalize_terms(scope[[field]])
  observed <- .lator_normalize_terms(context[[field]])
  if (!length(accepted)) return(list(blockers = blockers, warnings = warnings))
  if (!length(observed)) {
    message <- paste0(field, "_missing")
    if (isTRUE(missing_is_blocker)) blockers <- c(blockers, message) else warnings <- c(warnings, message)
  } else if (!length(intersect(accepted, observed))) {
    blockers <- c(blockers, paste0(field, "_outside_qualification"))
  }
  list(blockers = blockers, warnings = warnings)
}

.lator_evidence_score <- function(evidence) {
  collect <- function(value, prefix = "") {
    if (!is.list(value)) return(numeric())
    output <- numeric()
    for (name in names(value)) {
      item <- value[[name]]
      full <- paste(prefix, name, sep = ".")
      if (is.list(item)) output <- c(output, collect(item, full))
      else if (grepl("score$|confidence$|quality$", tolower(name))) {
        number <- suppressWarnings(as.numeric(item))
        if (length(number) == 1L && is.finite(number) && number >= 0 && number <= 1) {
          output[full] <- number
        }
      }
    }
    output
  }
  values <- collect(evidence)
  if (!length(values)) 0.5 else mean(values)
}

.lator_score_candidate <- function(candidate, context, endpoint,
                                   allowed_status, predictive_score = NA_real_) {
  qualification <- candidate$qualification %||% list()
  scope <- qualification$scope %||% list()
  blockers <- character()
  warnings <- character()
  status <- as.character(qualification$status %||% "unreviewed")
  if (!status %in% allowed_status) blockers <- c(blockers, "qualification_status_not_allowed")
  if (identical(status, "qualified") &&
      .lator_review_overdue(qualification$governance$review_due %||% "")) {
    blockers <- c(blockers, "qualification_review_overdue")
  }
  if (!tolower(endpoint$drug) %in% .lator_normalize_terms(scope$drugs)) {
    blockers <- c(blockers, "drug_outside_qualification")
  }
  endpoint_ids <- .lator_normalize_terms(scope$endpoint_ids)
  endpoint_kinds <- .lator_normalize_terms(scope$endpoint_kinds)
  if (length(endpoint_ids) || length(endpoint_kinds)) {
    endpoint_definitions <- if (identical(endpoint$kind, "multi_endpoint")) {
      lapply(endpoint$rules$components, `[[`, "endpoint")
    } else list(endpoint)
    covered <- vapply(endpoint_definitions, function(definition) {
      tolower(definition$id) %in% endpoint_ids ||
        tolower(definition$kind) %in% endpoint_kinds
    }, logical(1))
    if (!all(covered)) {
      blockers <- c(blockers, "endpoint_outside_qualification")
    }
  } else {
    blockers <- c(blockers, "endpoint_scope_missing")
  }
  context$indications <- context$indication %||% ""
  for (field in c("indications", "routes", "formulations", "regimens")) {
    matched <- .lator_restricted_match(
      scope, context, field, blockers, warnings, missing_is_blocker = TRUE
    )
    blockers <- matched$blockers
    warnings <- matched$warnings
  }

  required <- as.character(scope$covariates$required %||% character())
  optional <- as.character(scope$covariates$optional %||% character())
  for (name in required) {
    if (is.null(.lator_context_covariate(context, name))) {
      blockers <- c(blockers, paste0("required_covariate_missing:", name))
    }
  }
  optional_present <- if (length(optional)) {
    mean(vapply(optional, function(name) {
      !is.null(.lator_context_covariate(context, name))
    }, logical(1)))
  } else 1
  domain_score <- 1
  ranges <- c(scope$population %||% list(), scope$covariates$ranges %||% list())
  if (length(ranges)) for (name in names(ranges)) {
    rule <- ranges[[name]]
    covariate_name <- as.character(rule$covariate %||% "")[[1L]]
    if (!nzchar(covariate_name)) covariate_name <- name
    observed <- .lator_context_covariate(context, covariate_name)
    if (is.null(observed)) {
      if (isTRUE(rule$required)) {
        blockers <- c(blockers, paste0("domain_covariate_missing:", covariate_name))
      } else {
        warnings <- c(warnings, paste0("domain_covariate_unverified:", covariate_name))
        domain_score <- domain_score - 0.1
      }
      next
    }
    expected_unit <- tolower(trimws(as.character(rule$unit %||% "")))
    observed_unit <- tolower(trimws(as.character(observed$unit %||% "")))
    if (nzchar(expected_unit) && nzchar(observed_unit) &&
        !identical(expected_unit, observed_unit)) {
      blockers <- c(blockers, paste0("covariate_unit_mismatch:", covariate_name))
      next
    }
    minimum <- as.numeric(rule$min %||% -Inf)
    maximum <- as.numeric(rule$max %||% Inf)
    outside <- observed$value < minimum || observed$value > maximum
    if (outside && !identical(rule$hard, FALSE)) {
      blockers <- c(blockers, paste0("outside_validated_range:", covariate_name))
    } else if (outside) {
      warnings <- c(warnings, paste0("soft_extrapolation:", covariate_name))
      domain_score <- domain_score - 0.25
    }
  }
  assay <- scope$assays %||% list()
  if (isTRUE(assay$required) &&
      !length(.lator_normalize_terms(unlist(context$assays, use.names = FALSE)))) {
    blockers <- c(blockers, "assay_context_missing")
  }
  if (length(.lator_normalize_terms(context$assays$matrices)) &&
      length(.lator_normalize_terms(assay$matrices)) &&
      !length(intersect(
        .lator_normalize_terms(context$assays$matrices),
        .lator_normalize_terms(assay$matrices)
      ))) blockers <- c(blockers, "assay_matrix_outside_qualification")
  if (length(.lator_normalize_terms(context$assays$methods)) &&
      length(.lator_normalize_terms(assay$methods)) &&
      !length(intersect(
        .lator_normalize_terms(context$assays$methods),
        .lator_normalize_terms(assay$methods)
      ))) blockers <- c(blockers, "assay_method_outside_qualification")
  if (length(.lator_normalize_terms(context$assays$units)) &&
      length(.lator_normalize_terms(assay$units)) &&
      !length(intersect(
        .lator_normalize_terms(context$assays$units),
        .lator_normalize_terms(assay$units)
      ))) blockers <- c(blockers, "assay_unit_outside_qualification")

  components <- c(
    applicability = 1,
    population_domain = max(0, domain_score),
    validation_evidence = .lator_evidence_score(qualification$evidence %||% list()),
    covariate_completeness = optional_present
  )
  weights <- c(
    applicability = 0.35, population_domain = 0.25,
    validation_evidence = 0.30, covariate_completeness = 0.10
  )
  if (is.finite(predictive_score)) {
    predictive_score <- min(1, max(0, predictive_score))
    components <- c(components, patient_predictive = predictive_score)
    weights <- c(weights * 0.85, patient_predictive = 0.15)
  }
  score <- sum(components * weights[names(components)]) / sum(weights[names(components)])
  if (length(blockers)) score <- 0
  list(
    id = candidate$id,
    name = candidate$name %||% candidate$id,
    source = candidate$source %||% "local",
    model_version = candidate$model_version %||% "",
    model_hash = candidate$model_hash %||% "",
    qualification_id = qualification$qualification_id %||% "",
    qualification_status = status,
    eligible = !length(blockers),
    score = unname(score),
    blockers = unique(blockers),
    warnings = unique(c(warnings, qualification$limitations %||% character())),
    components = as.list(components),
    qualification = qualification
  )
}

#' Select the most suitable qualified population model
#'
#' Selection is deterministic. Hard applicability gates are evaluated before a
#' transparent suitability score. The function deliberately returns a no-match
#' or ambiguity result rather than silently choosing an unsupported model.
#'
#' @param patient Patient timeline.
#' @param endpoint Therapeutic endpoint.
#' @param candidates Candidate qualification records.
#' @param cutoff Latest patient time visible to the decision.
#' @param context_overrides Explicit context not represented on the timeline.
#' @param minimum_score Minimum score for automatic selection.
#' @param minimum_margin Required lead over the second candidate.
#' @param allowed_status Qualification statuses eligible for selection.
#' @param predictive_scores Optional named scores in `[0,1]`, derived from
#'   sequential patient-specific predictive assessment.
#' @return An immutable `lator_model_selection`.
#' @export
lator_model_select <- function(
    patient, endpoint, candidates, cutoff = Inf, context_overrides = list(),
    minimum_score = 0.65, minimum_margin = 0.05,
    allowed_status = "qualified", predictive_scores = NULL) {
  patient <- .lator_validate_patient(patient)
  endpoint <- lator_endpoint_validate(endpoint)
  if (!is.list(candidates)) .lator_stop("`candidates` must be a list.")
  minimum_score <- .lator_number(minimum_score, "minimum_score")
  minimum_margin <- .lator_number(minimum_margin, "minimum_margin")
  if (minimum_score < 0 || minimum_score > 1 ||
      minimum_margin < 0 || minimum_margin > 1) {
    .lator_stop("Selection score and margin must be between zero and one.")
  }
  context <- lator_patient_context(patient, endpoint, cutoff, context_overrides)
  scored <- lapply(candidates, function(candidate) {
    id <- as.character(candidate$id %||% "")
    predictive <- if (!is.null(predictive_scores) && id %in% names(predictive_scores)) {
      suppressWarnings(as.numeric(predictive_scores[[id]]))
    } else NA_real_
    .lator_score_candidate(candidate, context, endpoint, allowed_status, predictive)
  })
  if (length(scored)) {
    ordering <- order(
      -vapply(scored, `[[`, numeric(1), "score"),
      vapply(scored, `[[`, character(1), "id")
    )
    scored <- scored[ordering]
  }
  eligible <- Filter(function(candidate) {
    isTRUE(candidate$eligible) && candidate$score >= minimum_score
  }, scored)
  status <- "no_suitable_model"
  selected <- NULL
  reasons <- if (!length(scored)) "no_qualified_candidates" else
    if (!length(eligible)) "no_candidate_passed_gates_and_threshold" else character()
  if (length(eligible)) {
    lead <- if (length(eligible) > 1L) {
      eligible[[1L]]$score - eligible[[2L]]$score
    } else Inf
    if (length(eligible) > 1L && lead < minimum_margin) {
      status <- "multiple_suitable_models"
      reasons <- "top_candidates_within_ambiguity_margin"
    } else {
      status <- "selected"
      selected <- eligible[[1L]]
    }
  }
  selection <- structure(list(
    schema = "liberator.model_selection",
    schema_version = 1L,
    selection_id = .lator_id("model-selection"),
    status = status,
    created_at = .lator_now(),
    patient_id = patient$patient_id,
    patient_revision = patient$revision,
    context = context,
    context_hash = .lator_hash(context),
    endpoint = endpoint[c("id", "kind", "version", "drug")],
    endpoint_hash = .lator_hash(endpoint),
    criteria = list(
      minimum_score = minimum_score,
      minimum_margin = minimum_margin,
      allowed_status = allowed_status,
      scoring_version = "1.0.0"
    ),
    candidates = scored,
    selected_model_id = selected$id %||% "",
    selected_model_version = selected$model_version %||% "",
    selected_model_hash = selected$model_hash %||% "",
    selected_qualification_id = selected$qualification_id %||% "",
    reasons = reasons
  ), class = c("lator_model_selection", "list"))
  selection$selection_hash <- .lator_hash(unclass(selection))
  selection
}

#' Select a model from scoped LibeRary qualifications
#' @inheritParams lator_model_select
#' @param root Optional LibeRary catalogue root.
#' @return An immutable `lator_model_selection`.
#' @export
lator_model_select_from_liberary <- function(
    patient, endpoint, root = NULL, cutoff = Inf, context_overrides = list(),
    minimum_score = 0.65, minimum_margin = 0.05,
    allowed_status = "qualified", predictive_scores = NULL) {
  candidates <- lator_model_candidates_from_liberary(
    root = root, status = unique(c(allowed_status, "candidate"))
  )
  lator_model_select(
    patient = patient, endpoint = endpoint, candidates = candidates,
    cutoff = cutoff, context_overrides = context_overrides,
    minimum_score = minimum_score, minimum_margin = minimum_margin,
    allowed_status = allowed_status, predictive_scores = predictive_scores
  )
}

#' Persist an immutable model-selection decision
#'
#' @param workspace Unlocked workspace.
#' @param patient Patient record at the revision used by the selection.
#' @param selection Model-selection result.
#' @param actor Audit actor.
#' @return Saved patient record with incremented revision.
#' @export
lator_model_selection_save <- function(
    workspace, patient, selection, actor = "local-session") {
  workspace <- .lator_require_workspace(workspace)
  patient <- .lator_validate_patient(patient)
  if (!inherits(selection, "lator_model_selection") ||
      !identical(selection$schema, "liberator.model_selection") ||
      !selection$status %in% .lator_selection_statuses) {
    .lator_stop("Invalid LibeRator model-selection object.")
  }
  if (!identical(selection$patient_id, patient$patient_id) ||
      !identical(as.integer(selection$patient_revision), as.integer(patient$revision))) {
    .lator_stop("The model selection does not match this patient revision.")
  }
  patient$model_selections <- c(patient$model_selections %||% list(), list(selection))
  saved <- lator_patient_save(
    workspace, patient, expected_revision = patient$revision, actor = actor
  )
  .lator_audit_append(
    workspace, "model_selection_recorded", "patient", patient$patient_id,
    detail = list(
      selection_id = selection$selection_id,
      status = selection$status,
      selected_model_id = selection$selected_model_id,
      selection_hash = selection$selection_hash
    ),
    actor = actor
  )
  saved
}

#' @export
print.lator_model_selection <- function(x, ...) {
  cat("LibeRator model selection\n")
  cat("  status:", x$status, " candidates:", length(x$candidates), "\n")
  if (nzchar(x$selected_model_id %||% "")) {
    cat("  selected:", x$selected_model_id, " version:",
        x$selected_model_version %||% "", "\n")
  } else if (length(x$reasons)) {
    cat("  reason:", paste(x$reasons, collapse = ", "), "\n")
  }
  invisible(x)
}
