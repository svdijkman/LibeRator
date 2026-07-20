.lator_endpoint_kinds <- c(
  "therapeutic_range", "pre_event_target", "fraction_time_above_threshold",
  "auc_range", "trough_range", "custom"
)

#' Define a versioned therapeutic endpoint
#'
#' Endpoint definitions are evidence-bearing data, separate from estimation and
#' optimisation code. `draft` endpoints are suitable for exploration; a
#' `qualified` status is an explicit governance assertion by the user, not a
#' certification made by LibeRator.
#'
#' @param id Stable endpoint identifier.
#' @param name Display name.
#' @param drug Drug or intervention name.
#' @param kind Endpoint kind.
#' @param metric Metric evaluated against predicted trajectories.
#' @param unit Endpoint unit.
#' @param rules Named rule list interpreted by the endpoint evaluator.
#' @param source Citation or institutional-policy provenance.
#' @param status `"draft"`, `"reviewed"`, or `"qualified"`.
#' @param version Semantic version string.
#' @param metadata Additional non-executable metadata.
#' @return A serializable `lator_endpoint`.
#' @export
lator_endpoint <- function(id, name, drug, kind, metric, unit = "", rules = list(),
                           source = "", status = c("draft", "reviewed", "qualified"),
                           version = "1.0.0", metadata = list()) {
  endpoint <- structure(list(
    schema = "liberator.endpoint", schema_version = 1L,
    id = .lator_scalar(id, "id", max_chars = 128L),
    name = .lator_scalar(name, "name", max_chars = 256L),
    drug = .lator_scalar(drug, "drug", max_chars = 128L),
    kind = match.arg(as.character(kind), .lator_endpoint_kinds),
    metric = .lator_scalar(metric, "metric", max_chars = 128L),
    unit = .lator_scalar(unit, "unit", allow_empty = TRUE, max_chars = 64L),
    rules = rules, source = .lator_scalar(source, "source", allow_empty = TRUE, max_chars = 1000L),
    status = match.arg(status), version = .lator_scalar(version, "version", max_chars = 32L),
    metadata = metadata, created_at = .lator_now()
  ), class = "lator_endpoint")
  lator_endpoint_validate(endpoint)
}

#' Validate an endpoint definition
#' @param endpoint Endpoint object.
#' @return The validated endpoint.
#' @export
lator_endpoint_validate <- function(endpoint) {
  if (is.list(endpoint) && identical(endpoint$schema, "liberator.endpoint") &&
      !inherits(endpoint, "lator_endpoint")) class(endpoint) <- "lator_endpoint"
  if (!inherits(endpoint, "lator_endpoint") || !identical(endpoint$schema, "liberator.endpoint") ||
      as.integer(endpoint$schema_version) != 1L) .lator_stop("Invalid LibeRator endpoint.")
  if (!is.list(endpoint$rules) || !is.list(endpoint$metadata)) .lator_stop("Endpoint rules and metadata must be lists.")
  if (endpoint$kind %in% c("therapeutic_range", "auc_range", "trough_range")) {
    lower <- .lator_number(endpoint$rules$lower, "rules$lower")
    upper <- .lator_number(endpoint$rules$upper, "rules$upper")
    if (lower >= upper) .lator_stop("Endpoint lower bound must be below its upper bound.")
  }
  if (endpoint$kind == "fraction_time_above_threshold") {
    fraction <- .lator_number(endpoint$rules$target_fraction, "rules$target_fraction")
    if (fraction < 0 || fraction > 1) .lator_stop("Target fraction must be between zero and one.")
    multiplier <- .lator_number(endpoint$rules$threshold_multiplier %||% 1, "rules$threshold_multiplier", positive = TRUE)
    if (!is.null(endpoint$rules$free_fraction)) {
      free <- .lator_number(endpoint$rules$free_fraction, "rules$free_fraction", positive = TRUE)
      if (free > 1) .lator_stop("Free fraction cannot exceed one.")
    }
  }
  if (endpoint$kind == "pre_event_target") {
    targets <- endpoint$rules$targets
    required <- c("window_start", "window_end", "lower", "upper")
    if (!is.data.frame(targets) || !all(required %in% names(targets)) || !nrow(targets)) {
      .lator_stop("Pre-event endpoints require a non-empty target-window data frame.")
    }
    if (any(!is.finite(as.matrix(targets[required]))) || any(targets$window_start > targets$window_end) ||
        any(targets$lower > targets$upper)) .lator_stop("Pre-event target windows are invalid.")
  }
  endpoint
}

#' @export
print.lator_endpoint <- function(x, ...) {
  cat("LibeRator endpoint:", x$name, "\n")
  cat("  drug:", x$drug, " kind:", x$kind, " status:", x$status, "\n")
  cat("  version:", x$version, " source:", x$source %||% "", "\n")
  invisible(x)
}

#' AED therapeutic-range endpoint
#'
#' The optimisation target is the midpoint of the supplied range. No drug
#' ranges are hard-coded because ranges depend on assay, matrix, indication,
#' population, sampling time, and institutional policy.
#' @param drug Drug name.
#' @param lower,upper Therapeutic-range bounds.
#' @param unit Concentration unit.
#' @param source Evidence or policy provenance.
#' @param status Governance status.
#' @param metric Prediction metric; by default average concentration over the
#'   last dose interval.
#' @export
lator_endpoint_aed <- function(drug, lower, upper, unit, source,
                               status = c("draft", "reviewed", "qualified"),
                               metric = "last_interval_average") {
  drug <- .lator_scalar(drug, "drug")
  lator_endpoint(
    id = paste0("aed-", tolower(gsub("[^A-Za-z0-9]+", "-", drug))),
    name = paste(drug, "therapeutic range"), drug = drug,
    kind = "therapeutic_range", metric = metric, unit = unit,
    rules = list(lower = lower, upper = upper, target = mean(c(lower, upper))),
    source = source, status = match.arg(status),
    metadata = list(domain = "anti-epileptic-drug", target_policy = "range-midpoint")
  )
}

#' ATG pre-transplant target endpoint
#' @param drug ATG product/name.
#' @param targets Data frame with `window_start`, `window_end`, `lower`, and
#'   `upper`, expressed relative to the anchor in hours.
#' @param unit Concentration or exposure unit.
#' @param source Evidence or policy provenance.
#' @param anchor Patient procedure-event name used as time zero.
#' @param status Governance status.
#' @export
lator_endpoint_atg <- function(drug, targets, unit, source, anchor = "transplantation",
                               status = c("draft", "reviewed", "qualified")) {
  lator_endpoint(
    id = paste0("atg-", tolower(gsub("[^A-Za-z0-9]+", "-", drug))),
    name = paste(drug, "pre-transplant targets"), drug = drug,
    kind = "pre_event_target", metric = "window_concentration", unit = unit,
    rules = list(targets = as.data.frame(targets), anchor = anchor),
    source = source, status = match.arg(status), metadata = list(domain = "ATG")
  )
}

#' Beta-lactam fraction-of-time-above-MIC endpoint
#' @param drug Drug name.
#' @param target_fraction Required fraction of an interval above threshold.
#' @param mic_variable Covariate/event name containing MIC.
#' @param threshold_multiplier Threshold as a multiple of MIC.
#' @param free_fraction Fraction unbound; use one if predictions are already free concentrations.
#' @param source Evidence or policy provenance.
#' @param status Governance status.
#' @export
lator_endpoint_beta_lactam <- function(drug, target_fraction = 0.4,
                                       mic_variable = "MIC", threshold_multiplier = 1,
                                       free_fraction = 1, source,
                                       status = c("draft", "reviewed", "qualified")) {
  lator_endpoint(
    id = paste0("betalactam-", tolower(gsub("[^A-Za-z0-9]+", "-", drug))),
    name = paste(drug, "time above MIC"), drug = drug,
    kind = "fraction_time_above_threshold", metric = "fT>MIC", unit = "fraction",
    rules = list(target_fraction = target_fraction, mic_variable = mic_variable,
                 threshold_multiplier = threshold_multiplier, free_fraction = free_fraction),
    source = source, status = match.arg(status), metadata = list(domain = "beta-lactam")
  )
}

.lator_trapz <- function(time, value) {
  keep <- is.finite(time) & is.finite(value)
  time <- time[keep]; value <- value[keep]
  if (length(time) < 2L) return(NA_real_)
  ordering <- order(time); time <- time[ordering]; value <- value[ordering]
  sum(diff(time) * (value[-length(value)] + value[-1L]) / 2)
}

.lator_time_above <- function(time, concentration, threshold) {
  keep <- is.finite(time) & is.finite(concentration) & is.finite(threshold)
  time <- time[keep]; concentration <- concentration[keep]; threshold <- threshold[keep]
  if (length(time) < 2L || diff(range(time)) <= 0) return(NA_real_)
  ordering <- order(time); time <- time[ordering]
  difference <- concentration[ordering] - threshold[ordering]
  total <- 0
  for (index in seq_len(length(time) - 1L)) {
    duration <- time[index + 1L] - time[index]
    left <- difference[index]; right <- difference[index + 1L]
    fraction <- if (left >= 0 && right >= 0) 1 else if (left < 0 && right < 0) 0 else {
      crossing <- abs(left) / (abs(left) + abs(right))
      if (left >= 0) crossing else 1 - crossing
    }
    total <- total + duration * fraction
  }
  total / diff(range(time))
}

.lator_prediction_columns <- function(predictions) {
  time <- intersect(c("TIME", "time"), names(predictions))[1L]
  value <- intersect(c("IPRED", "PRED", "DV", "value", "concentration"), names(predictions))[1L]
  if (is.na(time) || is.na(value)) .lator_stop("Predictions require TIME/time and IPRED/PRED/value columns.")
  list(time = time, value = value)
}

#' Evaluate predictions against a therapeutic endpoint
#' @param endpoint Endpoint definition.
#' @param predictions Prediction data frame. Optional `SIM` identifies uncertainty replicates.
#' @param patient Patient timeline, required when an endpoint obtains MIC or an event anchor from it.
#' @param interval Optional two-element evaluation interval.
#' @return Per-replicate metrics and an aggregate target-attainment summary.
#' @export
lator_endpoint_evaluate <- function(endpoint, predictions, patient = NULL, interval = NULL) {
  endpoint <- lator_endpoint_validate(endpoint)
  predictions <- as.data.frame(predictions)
  columns <- .lator_prediction_columns(predictions)
  predictions$.lator_time <- as.numeric(predictions[[columns$time]])
  predictions$.lator_value <- as.numeric(predictions[[columns$value]])
  predictions$.lator_sim <- if ("SIM" %in% names(predictions)) predictions$SIM else 1L
  if (!is.null(interval)) {
    interval <- as.numeric(interval)
    if (length(interval) != 2L || any(!is.finite(interval)) || interval[1L] >= interval[2L]) .lator_stop("`interval` is invalid.")
    predictions <- predictions[predictions$.lator_time >= interval[1L] & predictions$.lator_time <= interval[2L], , drop = FALSE]
  }
  if (!nrow(predictions)) .lator_stop("No predictions remain in the endpoint evaluation interval.")
  groups <- split(predictions, predictions$.lator_sim)

  evaluate_one <- function(data) {
    time <- data$.lator_time; value <- data$.lator_value
    if (endpoint$kind == "therapeutic_range") {
      metric <- if (identical(endpoint$metric, "trough")) min(value, na.rm = TRUE) else {
        duration <- diff(range(time))
        if (duration > 0) .lator_trapz(time, value) / duration else utils::tail(value, 1L)
      }
      lower <- endpoint$rules$lower; upper <- endpoint$rules$upper; target <- endpoint$rules$target
      return(c(metric = metric, attained = metric >= lower && metric <= upper,
               score = abs(metric - target) / (upper - lower)))
    }
    if (endpoint$kind == "trough_range") {
      metric <- min(value, na.rm = TRUE); lower <- endpoint$rules$lower; upper <- endpoint$rules$upper
      return(c(metric = metric, attained = metric >= lower && metric <= upper,
               score = abs(metric - mean(c(lower, upper))) / (upper - lower)))
    }
    if (endpoint$kind == "auc_range") {
      metric <- .lator_trapz(time, value); lower <- endpoint$rules$lower; upper <- endpoint$rules$upper
      return(c(metric = metric, attained = metric >= lower && metric <= upper,
               score = abs(metric - mean(c(lower, upper))) / (upper - lower)))
    }
    if (endpoint$kind == "fraction_time_above_threshold") {
      if (is.null(patient)) .lator_stop("A patient timeline is required to resolve MIC.")
      resolved <- lator_covariate_at(
        patient, endpoint$rules$mic_variable, time, method = "locf", max_age = Inf
      )
      if (any(!is.finite(resolved$value))) .lator_stop("MIC is unresolved for part of the evaluation interval.")
      threshold <- resolved$value * endpoint$rules$threshold_multiplier
      effective <- value * endpoint$rules$free_fraction
      metric <- .lator_time_above(time, effective, threshold)
      target <- endpoint$rules$target_fraction
      return(c(metric = metric, attained = metric >= target, score = max(0, target - metric)))
    }
    if (endpoint$kind == "pre_event_target") {
      if (is.null(patient)) .lator_stop("A patient timeline is required to resolve the procedure anchor.")
      anchors <- .lator_active_events(patient, types = "procedure", name = endpoint$rules$anchor)
      if (!length(anchors)) .lator_stop("The endpoint's procedure anchor is absent from the patient timeline.")
      anchor <- utils::tail(vapply(anchors, `[[`, numeric(1), "time"), 1L)
      targets <- endpoint$rules$targets
      window_values <- vapply(seq_len(nrow(targets)), function(index) {
        inside <- time >= anchor + targets$window_start[index] & time <= anchor + targets$window_end[index]
        if (!any(inside)) return(NA_real_)
        mean(value[inside], na.rm = TRUE)
      }, numeric(1))
      attained <- all(is.finite(window_values) & window_values >= targets$lower & window_values <= targets$upper)
      width <- pmax(targets$upper - targets$lower, .Machine$double.eps)
      score <- mean(abs(window_values - (targets$lower + targets$upper) / 2) / width, na.rm = TRUE)
      return(c(metric = mean(window_values, na.rm = TRUE), attained = attained, score = score))
    }
    .lator_stop("Custom endpoint evaluation requires a registered evaluator and is not executable by default.")
  }
  result <- as.data.frame(do.call(rbind, lapply(groups, evaluate_one)))
  result$SIM <- names(groups); rownames(result) <- NULL
  list(
    endpoint_id = endpoint$id, endpoint_version = endpoint$version,
    results = result, attainment_probability = mean(as.logical(result$attained)),
    median_metric = stats::median(result$metric), median_score = stats::median(result$score),
    evaluated_at = .lator_now()
  )
}

.lator_builtin_endpoints <- function() list(
  list(id = "template-aed-range", name = "AED therapeutic-range template", kind = "therapeutic_range",
       description = "Provide drug-specific lower/upper bounds and provenance with lator_endpoint_aed()."),
  list(id = "template-atg-pre-event", name = "ATG pre-transplant target template", kind = "pre_event_target",
       description = "Provide evidence-based target windows relative to transplantation."),
  list(id = "template-betalactam-ftmic", name = "Beta-lactam fT>MIC template", kind = "fraction_time_above_threshold",
       description = "Provide target fraction, MIC policy, unbound fraction and provenance.")
)

#' List built-in templates and registered endpoint definitions
#' @param workspace Optional unlocked workspace.
#' @export
lator_endpoint_library <- function(workspace = NULL) {
  builtins <- do.call(rbind, lapply(.lator_builtin_endpoints(), as.data.frame, stringsAsFactors = FALSE))
  if (is.null(workspace)) return(builtins)
  workspace <- .lator_require_workspace(workspace)
  catalog <- .lator_encrypt_read(file.path(workspace$paths$endpoints, "catalog.enc"), workspace$key, list(items = list()))
  if (!length(catalog$items)) return(builtins)
  registered <- do.call(rbind, lapply(catalog$items, function(item) data.frame(
    id = item$id, name = item$name, kind = item$kind,
    description = paste(item$drug, item$status, paste0("v", item$version)), stringsAsFactors = FALSE
  )))
  rbind(builtins, registered)
}

#' Register an encrypted endpoint definition
#' @param workspace Unlocked workspace.
#' @param endpoint Endpoint to register.
#' @param actor Audit actor.
#' @export
lator_endpoint_register <- function(workspace, endpoint, actor = "local-session") {
  workspace <- .lator_require_workspace(workspace)
  endpoint <- lator_endpoint_validate(endpoint)
  .lator_with_lock(workspace, "workspace-write", function() {
    token <- .lator_record_token(paste(endpoint$id, endpoint$version, sep = "@"), workspace$key)
    path <- file.path(workspace$paths$endpoints, paste0(token, ".enc"))
    .lator_atomic_encrypt_save(endpoint, path, workspace$key)
    catalog_path <- file.path(workspace$paths$endpoints, "catalog.enc")
    catalog <- .lator_encrypt_read(catalog_path, workspace$key, list(items = list()))
    key <- paste(endpoint$id, endpoint$version, sep = "@")
    catalog$items[[key]] <- endpoint[c("id", "name", "drug", "kind", "status", "version", "source")]
    .lator_atomic_encrypt_save(catalog, catalog_path, workspace$key)
    .lator_audit_append(workspace, "endpoint_registered", "endpoint", key,
                        list(status = endpoint$status, hash = .lator_hash(endpoint)), actor)
    invisible(endpoint)
  })
}

.lator_endpoint_get <- function(workspace, id, version = NULL) {
  workspace <- .lator_require_workspace(workspace)
  catalog_path <- file.path(workspace$paths$endpoints, "catalog.enc")
  catalog <- .lator_encrypt_read(catalog_path, workspace$key, list(items = list()))
  keys <- names(catalog$items)
  matched <- keys[vapply(catalog$items, function(item) {
    identical(item$id, id) && (is.null(version) || identical(item$version, version))
  }, logical(1))]
  if (!length(matched)) .lator_stop("Unknown registered endpoint: ", id)
  key <- utils::tail(sort(matched), 1L)
  token <- .lator_record_token(key, workspace$key)
  lator_endpoint_validate(.lator_encrypt_read(
    file.path(workspace$paths$endpoints, paste0(token, ".enc")), workspace$key, NULL
  ))
}

.lator_registered_endpoints <- function(workspace) {
  catalog <- .lator_encrypt_read(file.path(workspace$paths$endpoints, "catalog.enc"),
                                 workspace$key, list(items = list()))
  if (!length(catalog$items)) return(list())
  result <- lapply(catalog$items, function(item) .lator_endpoint_get(workspace, item$id, item$version))
  names(result) <- names(catalog$items)
  result
}
