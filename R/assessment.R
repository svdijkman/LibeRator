.lator_match_therapy_events <- function(patient, types, analyte) {
  events <- .lator_active_events(patient, types = types)
  if (!length(events)) return(events)
  matched <- Filter(function(event) {
    event_drug <- as.character(event$metadata$drug %||% event$name %||% "")
    identical(tolower(trimws(event_drug)), tolower(trimws(analyte)))
  }, events)
  if (!length(matched)) {
    names <- unique(tolower(vapply(events, function(event) as.character(event$metadata$drug %||% event$name), character(1))))
    if (length(names) == 1L) matched <- events
  }
  matched
}

.lator_boundary_times <- function(patient, cutoff, state_times = NULL) {
  if (!is.null(state_times)) {
    times <- suppressWarnings(as.numeric(state_times))
    if (any(!is.finite(times))) .lator_stop("`state_times` must contain finite patient-timeline hours.")
    return(sort(unique(times[times <= cutoff])))
  }
  boundaries <- .lator_active_events(patient, types = "state_boundary", cutoff = cutoff)
  sort(unique(vapply(boundaries, `[[`, numeric(1), "time")))
}

.lator_patient_dataset <- function(patient, model, analyte, cutoff = Inf,
                                   covariate_policies = list(), dynamic = FALSE,
                                   state_times = NULL, include_future = NULL) {
  patient <- .lator_validate_patient(patient)
  doses <- .lator_match_therapy_events(patient, "dose", analyte)
  observations <- .lator_match_therapy_events(patient, "concentration", analyte)
  doses <- Filter(function(event) event$time <= cutoff, doses)
  observations <- Filter(function(event) event$time <= cutoff, observations)
  if (!length(observations) && is.null(include_future)) .lator_stop("No concentration observations match `", analyte, "`.")
  boundaries <- if (dynamic) .lator_boundary_times(patient, cutoff, state_times) else numeric()
  # The first epoch starts before any boundary. A boundary creates a new ETA state.
  boundaries <- boundaries[boundaries > min(c(
    vapply(c(doses, observations), `[[`, numeric(1), "time"), Inf
  ))]

  rows <- list()
  add_row <- function(time, role, event = NULL, old_occasion = NA_integer_) {
    metadata <- event$metadata %||% list()
    type <- event$type %||% "state_boundary"
    row <- data.frame(
      ID = patient$patient_id, TIME = time,
      EVID = if (type == "dose") as.integer(metadata$evid %||% 1L) else if (type == "concentration") 0L else 2L,
      AMT = if (type == "dose") as.numeric(event$value) else 0,
      RATE = if (type == "dose") as.numeric(metadata$rate %||% 0) else 0,
      II = if (type == "dose") as.numeric(metadata$ii %||% 0) else 0,
      SS = if (type == "dose") as.integer(metadata$ss %||% 0L) else 0L,
      ADDL = if (type == "dose") as.integer(metadata$addl %||% 0L) else 0L,
      CMT = if (type == "dose") as.integer(metadata$cmt %||% model$DOSECMP) else as.integer(model$OBSCMP),
      DV = if (type == "concentration") suppressWarnings(as.numeric(event$value)) else NA_real_,
      MDV = if (type == "concentration" && is.finite(suppressWarnings(as.numeric(event$value)))) 0L else 1L,
      .LATOR_ROLE = role, .LATOR_EVENT_ID = event$event_id %||% "",
      stringsAsFactors = FALSE
    )
    if (dynamic) {
      row$OCC <- if (is.finite(old_occasion)) old_occasion else 1L + findInterval(time, boundaries)
    }
    rows[[length(rows) + 1L]] <<- row
  }

  # Duplicate change rows ensure the old state/covariate propagates exactly to
  # the boundary; the zero-duration second row activates the new value.
  first_model_time <- min(vapply(c(doses, observations), `[[`, numeric(1), "time"), Inf)
  change_times <- sort(unique(c(
    boundaries,
    vapply(.lator_active_events(patient, "covariate", cutoff = cutoff), `[[`, numeric(1), "time")
  )))
  change_times <- change_times[change_times > first_model_time]
  for (time in change_times) {
    old_occ <- if (dynamic) max(1L, findInterval(time, boundaries, left.open = TRUE) + 1L) else NA_integer_
    add_row(time, "prechange", old_occasion = old_occ)
    add_row(time, "postchange")
  }
  for (event in doses) add_row(event$time, "dose", event)
  for (event in observations) add_row(event$time, "observation", event)
  if (!is.null(include_future)) {
    future <- as.data.frame(include_future)
    for (index in seq_len(nrow(future))) {
      evid <- as.integer(future$EVID[index] %||% 0L)
      event <- list(
        type = if (is.finite(evid) && evid != 0L) "dose" else "concentration",
        value = if (is.finite(evid) && evid != 0L) future$AMT[index] else NA_real_,
        metadata = list(
          evid = evid, rate = future$RATE[index] %||% 0,
          ii = future$II[index] %||% 0, ss = future$SS[index] %||% 0L,
          addl = future$ADDL[index] %||% 0L, cmt = future$CMT[index] %||% model$DOSECMP
        ), event_id = ""
      )
      add_row(as.numeric(future$TIME[index]), "future", event)
    }
  }
  if (!length(rows)) .lator_stop("The patient timeline contains no usable model events.")
  data <- do.call(rbind, rows)
  priority <- match(data$.LATOR_ROLE, c("prechange", "postchange", "dose", "observation", "future"))
  data <- data[order(data$TIME, priority), , drop = FALSE]
  rownames(data) <- NULL

  covariates <- .lator_resolve_covariates(
    patient, model$COVARIATES, data$TIME, covariate_policies, cutoff
  )
  for (name in model$COVARIATES) data[[name]] <- covariates$data[[name]]
  # A pre-change row must use the previous effective covariate value. This is
  # distinct from LOCF at the exact time, which correctly selects the new value.
  prechange <- data$.LATOR_ROLE == "prechange"
  if (any(prechange) && length(model$COVARIATES)) for (name in model$COVARIATES) {
    policy <- covariate_policies[[name]] %||% covariate_policies[[toupper(name)]] %||% list(method = "locf")
    earlier <- do.call(lator_covariate_at, c(
      list(patient = patient, name = name, times = data$TIME[prechange] - sqrt(.Machine$double.eps), cutoff = cutoff),
      policy
    ))
    data[[name]][prechange] <- earlier$value
  }
  unresolved <- model$COVARIATES[vapply(model$COVARIATES, function(name) any(!is.finite(data[[name]])), logical(1))]
  if (length(unresolved)) .lator_stop(
    "Required model covariates remain unresolved: ", paste(unresolved, collapse = ", "),
    ". Add evidence or an explicit fallback policy."
  )
  list(data = data, evidence = covariates$evidence, warnings = covariates$warnings,
       boundaries = boundaries)
}

.lator_random_walk_covariance <- function(model, occasions, process_scale = 0.1,
                                          process_covariance = NULL) {
  base <- .lator_omega_matrix(model)
  if (!nrow(base)) return(base)
  process <- if (is.null(process_covariance)) base * .lator_number(process_scale, "process_scale") else as.matrix(process_covariance)
  if (!identical(dim(process), dim(base)) || any(!is.finite(process))) {
    .lator_stop("`process_covariance` must match the model OMEGA dimensions.")
  }
  if (min(eigen((process + t(process)) / 2, symmetric = TRUE, only.values = TRUE)$values) < -1e-10) {
    .lator_stop("`process_covariance` must be positive semidefinite.")
  }
  output <- matrix(0, model$n_eta * occasions, model$n_eta * occasions)
  for (left in seq_len(occasions)) for (right in seq_len(occasions)) {
    rows <- (left - 1L) * model$n_eta + seq_len(model$n_eta)
    columns <- (right - 1L) * model$n_eta + seq_len(model$n_eta)
    output[rows, columns] <- base + (min(left, right) - 1L) * process
  }
  output
}

.lator_eta_trajectory <- function(fit, base_n_eta, boundaries) {
  occasions <- max(1L, length(fit$eta) / base_n_eta)
  starts <- c(-Inf, boundaries)
  if (length(starts) < occasions) starts <- c(starts, rep(utils::tail(starts, 1L), occasions - length(starts)))
  do.call(rbind, lapply(seq_len(occasions), function(occasion) {
    index <- (occasion - 1L) * base_n_eta + seq_len(base_n_eta)
    data.frame(
      occasion = occasion, start_time = starts[occasion],
      parameter = paste0("ETA", seq_len(base_n_eta)), estimate = fit$eta[index],
      standard_error = fit$eta_sd[index], stringsAsFactors = FALSE
    )
  }))
}

#' Longitudinal Bayesian patient assessment
#'
#' Each call creates an immutable assessment tied to hashes of its patient
#' evidence, model, endpoint, covariate policy, and cutoff. `dynamic` mode fits
#' occasion-specific ETA states under a Gaussian random-walk prior while the
#' compartment state remains continuous across boundaries.
#'
#' @param patient Longitudinal patient record.
#' @param model LibeRation population model.
#' @param endpoint Versioned therapeutic endpoint.
#' @param analyte Event drug/analyte name; defaults to `endpoint$drug`.
#' @param cutoff Patient-timeline hour through which evidence is visible.
#' @param mode `"static"` or `"dynamic"` latent patient parameters.
#' @param state_times Optional dynamic-state boundaries; otherwise explicit
#'   `state_boundary` events are used.
#' @param covariate_policies Named per-covariate policy lists passed to
#'   [lator_covariate_at()].
#' @param process_scale Random-walk innovation covariance as a multiple of OMEGA.
#' @param process_covariance Optional explicit innovation covariance.
#' @param workspace Optional workspace. When supplied, the immutable assessment
#'   is appended to and saves the patient record.
#' @param actor Audit actor used when saving.
#' @param ... Additional arguments to [LibeRation::nm_individual_fit()].
#' @export
lator_assess <- function(patient, model, endpoint, analyte = endpoint$drug,
                         cutoff = Inf, mode = c("static", "dynamic"),
                         state_times = NULL, covariate_policies = list(),
                         process_scale = 0.1, process_covariance = NULL,
                         workspace = NULL, actor = "local-session", ...) {
  started <- proc.time()[["elapsed"]]
  patient <- .lator_validate_patient(patient)
  endpoint <- lator_endpoint_validate(endpoint)
  if (!inherits(model, "nm_model")) .lator_stop("`model` must be a LibeRation nm_model.")
  mode <- match.arg(mode)
  cutoff <- .lator_number(cutoff, "cutoff", finite = FALSE)
  prepared_model <- if (mode == "dynamic") .lator_dynamic_model(model) else model
  prepared <- .lator_patient_dataset(
    patient, prepared_model, analyte, cutoff, covariate_policies,
    dynamic = mode == "dynamic", state_times = state_times
  )
  fit_arguments <- list(model = prepared_model, data = prepared$data)
  if (mode == "dynamic") {
    occasions <- max(prepared$data$OCC)
    fit_arguments$prior_mean <- rep(0, model$n_eta * occasions)
    fit_arguments$prior_covariance <- .lator_random_walk_covariance(
      model, occasions, process_scale, process_covariance
    )
  }
  fit <- do.call(LibeRation::nm_individual_fit, c(fit_arguments, list(...)))
  trajectory <- .lator_eta_trajectory(fit, model$n_eta, prepared$boundaries)
  current_endpoint <- tryCatch(
    lator_endpoint_evaluate(endpoint, fit$predictions, patient = patient),
    error = function(error) list(error = conditionMessage(error))
  )
  assessment <- structure(list(
    schema = "liberator.assessment", version = 1L, assessment_id = .lator_id("assessment"),
    patient_id = patient$patient_id, patient_revision = patient$revision,
    created_at = .lator_now(), cutoff = cutoff, mode = mode, analyte = analyte,
    evidence_hash = .lator_hash(.lator_active_events(patient, cutoff = cutoff)),
    model_hash = .lator_hash(model), endpoint_hash = .lator_hash(endpoint),
    policy_hash = .lator_hash(covariate_policies), endpoint = endpoint,
    model_provenance = attr(model, "library_provenance", exact = TRUE) %||% list(),
    eta = fit$eta, eta_covariance = fit$eta_covariance, eta_trajectory = trajectory,
    predictions = fit$predictions, data = fit$data, covariate_policies = covariate_policies,
    covariate_evidence = prepared$evidence,
    endpoint_evaluation = current_endpoint, convergence = fit$convergence,
    warnings = unique(prepared$warnings), diagnostics = c(fit$diagnostics, list(
      gradient_max = if (length(fit$gradient)) max(abs(fit$gradient)) else NA_real_,
      elapsed_total_seconds = unname(proc.time()[["elapsed"]] - started)
    )), model = prepared_model
  ), class = "lator_assessment")
  assessment$assessment_hash <- .lator_hash(assessment)
  if (!is.null(workspace)) {
    patient$assessments <- c(patient$assessments, list(assessment))
    saved <- lator_patient_save(workspace, patient, expected_revision = patient$revision, actor = actor)
    assessment$patient_revision <- saved$revision
  }
  assessment
}

#' @export
print.lator_assessment <- function(x, ...) {
  cat("LibeRator patient assessment\n")
  cat("  id:", x$assessment_id, " mode:", x$mode, " cutoff:", format(x$cutoff), "\n")
  cat("  latent states:", length(unique(x$eta_trajectory$occasion)),
      " ETA estimates:", nrow(x$eta_trajectory), " convergence:", x$convergence, "\n")
  if (length(x$warnings)) cat("  warnings:", paste(x$warnings, collapse = "; "), "\n")
  invisible(x)
}
