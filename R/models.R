.lator_model_arguments <- function(model, iov = model$IOV, occasion_col = model$LIK_CONFIG$occasion_col) {
  if (!inherits(model, "nm_model")) .lator_stop("`model` must be a LibeRation nm_model.")
  config <- unclass(model$LIK_CONFIG)
  config$version <- NULL
  supported <- names(formals(LibeRation::nm_lik_config))
  config <- config[intersect(names(config), supported)]
  config$iov <- as.integer(iov)
  config$occasion_col <- occasion_col
  config <- do.call(LibeRation::nm_lik_config, config)
  list(
    INPUT = unique(c(model$INPUT, if (iov > 0L) occasion_col else character())),
    ADVAN = model$ADVAN, TRANS = model$TRANS, SS = model$SS,
    DOSECMP = model$DOSECMP, OBSCMP = model$OBSCMP,
    PRED = model$PRED, ERROR = model$ERROR, DES = model$DES,
    THETAS = model$THETAS, OMEGAS = model$OMEGAS, SIGMAS = model$SIGMAS,
    COVARIATES = model$COVARIATES, USE_ODE = model$USE_ODE,
    ODE_CONTROL = model$ODE_CONTROL, IOV = as.integer(iov), LIK_CONFIG = config,
    SOLVER = model$SOLVER, ERROR_TYPE = model$ERROR_TYPE,
    GRAPH = model$GRAPH, LAYOUT = model$LAYOUT, LANGUAGE = model$LANGUAGE
  )
}

.lator_dynamic_model <- function(model) {
  if (!inherits(model, "nm_model")) .lator_stop("`model` must be a LibeRation nm_model.")
  if (model$n_eta < 1L) .lator_stop("A time-varying patient state requires at least one ETA.")
  dynamic <- do.call(LibeRation::nm_model, .lator_model_arguments(model, model$n_eta, "OCC"))
  attr(dynamic, "lator_base_model_hash") <- .lator_hash(model)
  attr(dynamic, "lator_dynamic_state") <- TRUE
  dynamic
}

.lator_omega_matrix <- function(model) {
  output <- matrix(0, model$n_eta, model$n_eta)
  if (!model$n_eta) return(output)
  for (index in seq_len(nrow(model$OMEGAS))) {
    row <- model$OMEGAS$ROW[index]; column <- model$OMEGAS$COL[index]
    output[row, column] <- output[column, row] <- model$OMEGAS$Value[index]
  }
  output
}

.lator_reject_known_model_defects <- function(model) {
  provenance <- attr(model, "library_provenance", exact = TRUE) %||% list()
  library_id <- as.character(provenance$library_id %||% "")
  code <- gsub("[[:space:]]", "", as.character(model$PRED %||% ""))
  if (identical(library_id, "aedapt_lamotrigine_he") &&
      grepl("V=THETA(6)*WT", code, fixed = TRUE)) {
    .lator_stop(
      "This saved He et al. lamotrigine model is the superseded LibeRary ",
      "1.0.0 translation with an incorrect volume relationship. Reopen ",
      "'Add/select model' and add He et al. v1.0.1 before individualisation."
    )
  }
  invisible(model)
}

.lator_model_profile_type <- function(model) {
  explicit <- as.character(
    attr(model, "lator_profile_type", exact = TRUE) %||% ""
  )
  if (length(explicit) == 1L &&
      explicit %in% c("time_course", "steady_state_mean_only")) {
    return(explicit)
  }
  provenance <- attr(model, "library_provenance", exact = TRUE) %||% list()
  if (identical(
    as.character(provenance$library_id %||% ""),
    "aedapt_clonazepam_yukawa"
  )) {
    return("steady_state_mean_only")
  }
  direct <- identical(model$SOLVER, "direct") ||
    identical(model$PRED_MODE, "pred")
  code <- paste(
    as.character(model$PRED %||% ""),
    as.character(model$ERROR %||% ""),
    collapse = ";"
  )
  temporal <- grepl(
    "\\b(TIME|TAD|TSLD|TIME_AFTER_DOSE|A[0-9]+)\\b",
    toupper(code), perl = TRUE
  )
  if (direct && !temporal) {
    return("steady_state_mean_only")
  }
  "time_course"
}

.lator_model_catalog_path <- function(workspace) file.path(workspace$paths$models, "catalog.enc")

.lator_selection_model_key <- function(selection) {
  paste0(
    "liberary-", selection$selected_model_id, "-",
    substr(selection$selected_qualification_id %||% "unqualified", 1L, 24L)
  )
}

#' Register an encrypted LibeRation model for individualisation
#' @param workspace Unlocked workspace.
#' @param model A LibeRation `nm_model`.
#' @param id Stable local model id.
#' @param name Display name.
#' @param qualification Named governance and validation metadata.
#' @param endpoint_ids Compatible endpoint identifiers.
#' @param provenance Evidence and import provenance.
#' @param actor Audit actor.
#' @export
lator_model_register <- function(workspace, model, id = NULL, name = NULL,
                                 qualification = list(status = "research"),
                                 endpoint_ids = character(), provenance = list(),
                                 actor = "local-session") {
  workspace <- .lator_require_workspace(workspace)
  if (!inherits(model, "nm_model")) .lator_stop("`model` must be a LibeRation nm_model.")
  id <- .lator_scalar(id %||% attr(model, "name", exact = TRUE) %||% .lator_id("model"), "id", max_chars = 128L)
  name <- .lator_scalar(name %||% attr(model, "name", exact = TRUE) %||% id, "name", max_chars = 256L)
  if (!is.list(qualification) || !is.list(provenance)) .lator_stop("Qualification and provenance must be lists.")
  registration <- list(
    schema = "liberator.model", version = 1L, id = id, name = name,
    model = model, model_hash = .lator_hash(model), endpoint_ids = unique(as.character(endpoint_ids)),
    qualification = qualification, provenance = provenance, registered_at = .lator_now()
  )
  .lator_with_lock(workspace, "workspace-write", function() {
    token <- .lator_record_token(id, workspace$key)
    .lator_atomic_encrypt_save(registration, file.path(workspace$paths$models, paste0(token, ".enc")), workspace$key)
    catalog <- .lator_encrypt_read(.lator_model_catalog_path(workspace), workspace$key, list(items = list()))
    catalog$items[[id]] <- registration[c("id", "name", "model_hash", "endpoint_ids", "qualification", "registered_at")]
    .lator_atomic_encrypt_save(catalog, .lator_model_catalog_path(workspace), workspace$key)
    .lator_audit_append(workspace, "model_registered", "model", id,
                        list(hash = registration$model_hash, qualification = qualification), actor)
    invisible(registration)
  })
}

.lator_model_get <- function(workspace, id) {
  workspace <- .lator_require_workspace(workspace)
  token <- .lator_record_token(.lator_scalar(id, "id"), workspace$key)
  value <- .lator_encrypt_read(file.path(workspace$paths$models, paste0(token, ".enc")), workspace$key, NULL)
  if (is.null(value)) .lator_stop("Unknown registered model: ", id)
  value
}

.lator_registered_models <- function(workspace) {
  catalog <- .lator_encrypt_read(.lator_model_catalog_path(workspace), workspace$key, list(items = list()))
  if (!length(catalog$items)) return(list())
  result <- lapply(names(catalog$items), function(id) {
    registration <- .lator_model_get(workspace, id)
    model <- registration$model %||% NULL
    if (!inherits(model, "nm_model")) {
      .lator_stop(
        "Registered model '", id,
        "' does not contain a valid LibeRation nm_model."
      )
    }
    if (is.null(attr(model, "name", exact = TRUE)) ||
        !nzchar(as.character(attr(model, "name", exact = TRUE)))) {
      attr(model, "name") <- registration$name %||% id
    }
    model
  })
  names(result) <- names(catalog$items)
  result
}

#' Import a LibeRary catalogue model
#'
#' Unvalidated catalogue entries are rejected by default. Enabling them is an
#' explicit research override retained in model provenance.
#' @param library_id LibeRary catalogue id.
#' @param root LibeRary catalogue root.
#' @param allow_unvalidated Permit an entry whose status is not `validated`.
#' @param qualification_id Optional current clinical-use qualification id to
#'   bind into model provenance.
#' @return A compiled, serializable LibeRation `nm_model` with catalogue provenance.
#' @export
lator_model_from_liberary <- function(library_id, root = NULL,
                                      allow_unvalidated = FALSE,
                                      qualification_id = NULL) {
  if (!requireNamespace("LibeRary", quietly = TRUE)) .lator_stop("Install LibeRary to import catalogue models.")
  library_id <- .lator_scalar(library_id, "library_id")
  get_args <- list(library_id = library_id)
  if (!is.null(root)) get_args$root <- root
  entry <- do.call(LibeRary::library_get, get_args)
  if (!isTRUE(entry$validation$valid)) .lator_stop("LibeRary entry validation failed: ", paste(entry$validation$errors, collapse = "; "))
  status <- as.character(entry$manifest$status %||% "")
  if (!identical(tolower(status), "validated") && !isTRUE(allow_unvalidated)) {
    .lator_stop("LibeRary entry '", library_id, "' has status '", status,
                "'. Set `allow_unvalidated = TRUE` only for an explicit research override.")
  }
  model_args <- list(library_id = library_id)
  if (!is.null(root)) model_args$root <- root
  control_lines <- do.call(LibeRary::library_model, model_args)
  control <- LibeRation::nm_control_read(control_lines, strict = TRUE)
  clinical_qualification <- NULL
  if (!is.null(qualification_id) && nzchar(as.character(qualification_id))) {
    qualifications <- .lator_liberary_api(
      "library_clinical_qualifications"
    )
    qualification_args <- list(
      library_id = library_id, current = TRUE
    )
    if (!is.null(root)) qualification_args$root <- root
    records <- do.call(qualifications, qualification_args)
    matched <- Filter(function(record) {
      identical(
        as.character(record$qualification_id),
        as.character(qualification_id)
      )
    }, records)
    if (length(matched) != 1L ||
        !identical(matched[[1L]]$status, "qualified")) {
      .lator_stop("The requested current qualified clinical-use record is unavailable.")
    }
    clinical_qualification <- matched[[1L]]
  }
  provenance <- list(
    source = "LibeRary", library_id = library_id,
    title = entry$manifest$title %||% library_id,
    library_version = entry$manifest$version %||% "", status_at_import = status,
    unvalidated_override = !identical(tolower(status), "validated"),
    imported_at = .lator_now(), evidence = entry$manifest$provenance %||% list(),
    study = entry$manifest$study %||% list(),
    confidence = entry$manifest$confidence %||% list(),
    model_metadata = entry$manifest$model %||% list(),
    qualification = entry$manifest$qualification %||% list(),
    clinical_qualification = clinical_qualification
  )
  attr(control$model, "name") <- entry$manifest$title %||% library_id
  attr(control$model, "library_provenance") <- provenance
  attr(control$model, "lator_parameter_labels") <- list(
    theta = .lator_control_parameter_labels(control_lines, "THETA")
  )
  control$model
}
