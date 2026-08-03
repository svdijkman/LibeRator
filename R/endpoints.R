.lator_endpoint_kinds <- c(
  "therapeutic_range", "pre_event_target", "fraction_time_above_threshold",
  "auc_range", "trough_range", "auc_mic_range", "peak_mic_safety",
  "timed_thresholds", "time_in_range", "multi_endpoint", "custom"
)

.lator_endpoint_component_roles <- c("primary", "secondary", "safety")

.lator_endpoint_qualification_check <- function(status, metadata) {
  if (!identical(status, "qualified")) return(invisible(TRUE))
  attestation <- metadata$qualification_attestation %||% list()
  required <- c("issuer", "reviewer", "reviewed_at", "evidence", "scope")
  missing <- required[!vapply(required, function(field) {
    value <- attestation[[field]] %||% ""
    length(value) == 1L && nzchar(trimws(as.character(value)))
  }, logical(1))]
  reviewed_at <- suppressWarnings(as.POSIXct(
    attestation$reviewed_at %||% NA_character_, tz = "UTC"
  ))
  if (length(missing) || is.na(reviewed_at) ||
      !isTRUE(metadata$research_acknowledged)) {
    .lator_stop(
      "A qualified endpoint requires metadata$qualification_attestation with ",
      "issuer, reviewer, reviewed_at, evidence, and scope, plus ",
      "metadata$research_acknowledged = TRUE. The acknowledgement confirms ",
      "that this LibeRator build remains research-only."
    )
  }
  invisible(TRUE)
}

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
  status <- match.arg(status)
  if (!is.list(metadata)) .lator_stop("Endpoint metadata must be a list.")
  .lator_endpoint_qualification_check(status, metadata)
  endpoint <- structure(list(
    schema = "liberator.endpoint", schema_version = 1L,
    id = .lator_scalar(id, "id", max_chars = 128L),
    name = .lator_scalar(name, "name", max_chars = 256L),
    drug = .lator_scalar(drug, "drug", max_chars = 128L),
    kind = match.arg(as.character(kind), .lator_endpoint_kinds),
    metric = .lator_scalar(metric, "metric", max_chars = 128L),
    unit = .lator_scalar(unit, "unit", allow_empty = TRUE, max_chars = 64L),
    rules = rules, source = .lator_scalar(source, "source", allow_empty = TRUE, max_chars = 1000L),
    status = status, version = .lator_scalar(version, "version", max_chars = 32L),
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
  .lator_endpoint_qualification_check(endpoint$status, endpoint$metadata)
  if (endpoint$kind == "multi_endpoint") {
    components <- endpoint$rules$components
    if (!is.list(components) || length(components) < 2L) {
      .lator_stop("A multi-endpoint objective requires at least two components.")
    }
    components <- lapply(components, function(component) {
      if (!is.list(component) || is.null(component$endpoint)) {
        .lator_stop("Every multi-endpoint component requires an endpoint definition.")
      }
      nested <- lator_endpoint_validate(component$endpoint)
      if (identical(nested$kind, "multi_endpoint")) {
        .lator_stop("Nested multi-endpoint objectives are not supported.")
      }
      role <- match.arg(
        as.character(component$role %||% "secondary"),
        .lator_endpoint_component_roles
      )
      weight <- .lator_number(
        component$weight %||% 1, "component$weight", positive = TRUE
      )
      hard <- isTRUE(component$hard_constraint)
      minimum <- if (hard) {
        .lator_number(
          component$minimum_attainment %||% 0.9,
          "component$minimum_attainment"
        )
      } else NA_real_
      if (hard && (minimum < 0 || minimum > 1)) {
        .lator_stop(
          "Hard-constraint minimum attainment must be between zero and one."
        )
      }
      list(
        component_id = as.character(
          component$component_id %||%
            paste(nested$id, nested$version, sep = "@")
        ),
        endpoint = nested, role = role, weight = weight,
        hard_constraint = hard, minimum_attainment = minimum
      )
    })
    ids <- vapply(components, `[[`, character(1), "component_id")
    if (any(!nzchar(ids)) || anyDuplicated(ids)) {
      .lator_stop("Multi-endpoint component identifiers must be unique.")
    }
    primary <- vapply(
      components, function(component) identical(component$role, "primary"),
      logical(1)
    )
    if (sum(primary) != 1L) {
      .lator_stop("A multi-endpoint objective requires exactly one primary component.")
    }
    drugs <- vapply(
      components, function(component) .lator_drug_key(component$endpoint$drug),
      character(1)
    )
    if (length(unique(drugs)) != 1L ||
        !identical(unique(drugs), .lator_drug_key(endpoint$drug))) {
      .lator_stop(
        "All multi-endpoint components must match the objective's medication."
      )
    }
    endpoint$rules$components <- components
    endpoint$rules$decision_policy <- as.character(
      endpoint$rules$decision_policy %||% "constrained_utility"
    )
    if (!endpoint$rules$decision_policy %in% "constrained_utility") {
      .lator_stop("Unknown multi-endpoint decision policy.")
    }
  }
  if (endpoint$kind %in% c("therapeutic_range", "auc_range", "trough_range")) {
    lower <- .lator_number(endpoint$rules$lower, "rules$lower")
    upper <- .lator_number(endpoint$rules$upper, "rules$upper")
    if (lower >= upper) .lator_stop("Endpoint lower bound must be below its upper bound.")
  }
  if (endpoint$kind == "auc_mic_range") {
    lower <- .lator_number(endpoint$rules$lower, "rules$lower")
    upper <- .lator_number(endpoint$rules$upper, "rules$upper")
    if (lower >= upper) .lator_stop("AUC/MIC lower bound must be below its upper bound.")
    .lator_scalar(endpoint$rules$mic_variable, "rules$mic_variable")
    endpoint$rules$mic_max_age <- .lator_number(
      endpoint$rules$mic_max_age %||% 168, "rules$mic_max_age",
      positive = TRUE
    )
    if (!is.null(endpoint$rules$safety_auc_upper)) {
      .lator_number(endpoint$rules$safety_auc_upper, "rules$safety_auc_upper", positive = TRUE)
    }
  }
  if (endpoint$kind == "peak_mic_safety") {
    .lator_number(endpoint$rules$efficacy_lower, "rules$efficacy_lower", positive = TRUE)
    .lator_number(endpoint$rules$trough_upper, "rules$trough_upper", positive = TRUE)
    if (!is.null(endpoint$rules$efficacy_upper)) {
      upper <- .lator_number(endpoint$rules$efficacy_upper, "rules$efficacy_upper", positive = TRUE)
      if (upper <= endpoint$rules$efficacy_lower) {
        .lator_stop("Peak/MIC upper bound must exceed its lower bound.")
      }
    }
    if (!endpoint$rules$efficacy_metric %in% c("Cmax/MIC", "AUC/MIC")) {
      .lator_stop("Unknown aminoglycoside efficacy metric.")
    }
    endpoint$rules$mic_max_age <- .lator_number(
      endpoint$rules$mic_max_age %||% 168, "rules$mic_max_age",
      positive = TRUE
    )
  }
  if (endpoint$kind == "fraction_time_above_threshold") {
    fraction <- .lator_number(endpoint$rules$target_fraction, "rules$target_fraction")
    if (fraction < 0 || fraction > 1) .lator_stop("Target fraction must be between zero and one.")
    multiplier <- .lator_number(endpoint$rules$threshold_multiplier %||% 1, "rules$threshold_multiplier", positive = TRUE)
    if (!is.null(endpoint$rules$free_fraction)) {
      free <- .lator_number(endpoint$rules$free_fraction, "rules$free_fraction", positive = TRUE)
      if (free > 1) .lator_stop("Free fraction cannot exceed one.")
    }
    endpoint$rules$mic_max_age <- .lator_number(
      endpoint$rules$mic_max_age %||% 168, "rules$mic_max_age",
      positive = TRUE
    )
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
  if (endpoint$kind == "timed_thresholds") {
    targets <- endpoint$rules$targets
    required <- c("hours_after_dose", "lower", "upper", "tolerance")
    if (!is.data.frame(targets) || !nrow(targets) ||
        !all(required %in% names(targets))) {
      .lator_stop("Timed endpoints require hours_after_dose, lower, upper, and tolerance.")
    }
    numeric_targets <- as.matrix(targets[required])
    if (any(is.na(numeric_targets)) ||
        any(targets$lower > targets$upper) ||
        any(targets$tolerance < 0)) {
      .lator_stop("Timed endpoint thresholds are invalid.")
    }
  }
  if (endpoint$kind == "time_in_range") {
    lower <- .lator_number(endpoint$rules$lower, "rules$lower")
    upper <- .lator_number(endpoint$rules$upper, "rules$upper")
    target <- .lator_number(endpoint$rules$target_fraction, "rules$target_fraction")
    if (lower >= upper || target < 0 || target > 1) {
      .lator_stop("Time-in-range rules are invalid.")
    }
  }
  endpoint
}

#' Define one component of a multi-endpoint objective
#'
#' @param endpoint A versioned [lator_endpoint()].
#' @param role `"primary"`, `"secondary"`, or `"safety"`.
#' @param weight Positive utility weight. Weights are normalized within the
#'   endpoint set, so only their relative values matter.
#' @param hard_constraint Whether candidate regimens must satisfy a minimum
#'   target-attainment probability across conditional ETA draws for this component.
#' @param minimum_attainment Required probability when `hard_constraint` is
#'   `TRUE`.
#' @param component_id Optional stable component identifier.
#' @return A serializable endpoint-component specification.
#' @export
lator_endpoint_component <- function(
    endpoint, role = c("primary", "secondary", "safety"), weight = 1,
    hard_constraint = FALSE, minimum_attainment = 0.9,
    component_id = NULL) {
  endpoint <- lator_endpoint_validate(endpoint)
  if (identical(endpoint$kind, "multi_endpoint")) {
    .lator_stop("Nested multi-endpoint objectives are not supported.")
  }
  role <- match.arg(role)
  weight <- .lator_number(weight, "weight", positive = TRUE)
  hard_constraint <- isTRUE(hard_constraint)
  minimum_attainment <- if (hard_constraint) {
    value <- .lator_number(minimum_attainment, "minimum_attainment")
    if (value < 0 || value > 1) {
      .lator_stop("`minimum_attainment` must be between zero and one.")
    }
    value
  } else NA_real_
  list(
    component_id = .lator_scalar(
      component_id %||% paste(endpoint$id, endpoint$version, sep = "@"),
      "component_id", max_chars = 128L
    ),
    endpoint = endpoint, role = role, weight = weight,
    hard_constraint = hard_constraint,
    minimum_attainment = minimum_attainment
  )
}

#' Combine therapeutic endpoints into a versioned clinical objective
#'
#' Candidate regimens are first screened against component-specific
#' conditional-draw chance constraints. Remaining candidates are ranked by expected normalized
#' clinical utility and annotated with joint target attainment and Pareto
#' status. The component weights and thresholds are explicit, versioned
#' clinical inputs rather than hidden application defaults.
#'
#' @param id Stable endpoint-set identifier.
#' @param name Display name.
#' @param drug Medication shared by all components.
#' @param components A list created with [lator_endpoint_component()].
#' @param source Clinical protocol, literature, or governance provenance.
#' @param status `"draft"`, `"reviewed"`, or `"qualified"`.
#' @param version Semantic version string.
#' @param metadata Additional non-executable metadata.
#' @return A versioned `lator_endpoint` with kind `"multi_endpoint"`.
#' @export
lator_endpoint_set <- function(
    id, name, drug, components, source,
    status = c("draft", "reviewed", "qualified"), version = "1.0.0",
    metadata = list()) {
  lator_endpoint(
    id = id, name = name, drug = drug, kind = "multi_endpoint",
    metric = "joint clinical utility", unit = "utility",
    rules = list(
      components = components,
      decision_policy = "constrained_utility"
    ),
    source = source, status = match.arg(status), version = version,
    metadata = c(
      list(
        objective_type = "multi-endpoint",
        utility_transform = "exp(-normalized_loss)"
      ),
      metadata
    )
  )
}

#' @export
print.lator_endpoint <- function(x, ...) {
  cat("LibeRator endpoint:", x$name, "\n")
  cat("  status: RESEARCH ONLY; not a clinical instruction\n")
  cat("  drug:", x$drug, " kind:", x$kind, " status:", x$status, "\n")
  cat("  version:", x$version, " source:", x$source %||% "", "\n")
  if (identical(x$kind, "multi_endpoint")) {
    cat("  components:", length(x$rules$components), "\n")
  }
  invisible(x)
}

#' AED therapeutic-range endpoint
#'
#' The optimisation target is the midpoint of the supplied range. No drug
#' constructor requires an explicit range because targets depend on assay,
#' matrix, indication, population, sampling time, and institutional policy.
#' The GUI may prefill an editable, provenance-labelled drug reference range
#' when one is available.
#' @param drug Drug name.
#' @param lower,upper Therapeutic-range bounds.
#' @param unit Concentration unit.
#' @param source Evidence or policy provenance.
#' @param status Governance status.
#' @param metric Prediction metric; by default average concentration over the
#'   last dose interval.
#' @param metadata Additional metadata, including qualification attestation
#'   when `status = "qualified"`.
#' @export
lator_endpoint_aed <- function(drug, lower, upper, unit, source,
                               status = c("draft", "reviewed", "qualified"),
                               metric = "last_interval_average", metadata = list()) {
  drug <- .lator_scalar(drug, "drug")
  lator_endpoint(
    id = paste0("aed-", tolower(gsub("[^A-Za-z0-9]+", "-", drug))),
    name = paste(drug, "therapeutic range"), drug = drug,
    kind = "therapeutic_range", metric = metric, unit = unit,
    rules = list(lower = lower, upper = upper, target = mean(c(lower, upper))),
    source = source, status = match.arg(status),
    metadata = c(list(domain = "anti-epileptic-drug", target_policy = "range-midpoint"), metadata)
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
#' @param metadata Additional metadata.
#' @export
lator_endpoint_atg <- function(drug, targets, unit, source, anchor = "transplantation",
                               status = c("draft", "reviewed", "qualified"), metadata = list()) {
  lator_endpoint(
    id = paste0("atg-", tolower(gsub("[^A-Za-z0-9]+", "-", drug))),
    name = paste(drug, "pre-transplant targets"), drug = drug,
    kind = "pre_event_target", metric = "window_concentration", unit = unit,
    rules = list(targets = as.data.frame(targets), anchor = anchor),
    source = source, status = match.arg(status), metadata = c(list(domain = "ATG"), metadata)
  )
}

#' Beta-lactam fraction-of-time-above-MIC endpoint
#' @param drug Drug name.
#' @param target_fraction Required fraction of an interval above threshold.
#' @param mic_variable Covariate/event name containing MIC.
#' @param threshold_multiplier Threshold as a multiple of MIC.
#' @param free_fraction Fraction unbound; use one if predictions are already free concentrations.
#' @param mic_max_age Maximum age in hours of MIC evidence. Stale evidence is
#'   rejected unless an audited evaluation-time override is supplied.
#' @param source Evidence or policy provenance.
#' @param status Governance status.
#' @param metadata Additional metadata.
#' @export
lator_endpoint_beta_lactam <- function(drug, target_fraction = 0.4,
                                       mic_variable = "MIC", threshold_multiplier = 1,
                                       free_fraction = 1, mic_max_age = 168, source,
                                       status = c("draft", "reviewed", "qualified"),
                                       metadata = list()) {
  lator_endpoint(
    id = paste0("betalactam-", tolower(gsub("[^A-Za-z0-9]+", "-", drug))),
    name = paste(drug, "time above MIC"), drug = drug,
    kind = "fraction_time_above_threshold", metric = "fT>MIC", unit = "fraction",
    rules = list(target_fraction = target_fraction, mic_variable = mic_variable,
                 threshold_multiplier = threshold_multiplier,
                 free_fraction = free_fraction, mic_max_age = mic_max_age),
    source = source, status = match.arg(status),
    metadata = c(list(domain = "beta-lactam"), metadata)
  )
}

#' Vancomycin AUC/MIC endpoint
#'
#' Target values are deliberately supplied by the caller so an institution can
#' bind the definition to its approved indication, MIC method, and protocol.
#' @param drug Drug name.
#' @param lower,upper AUC/MIC target bounds.
#' @param source Guideline or institutional-policy provenance.
#' @param mic_variable Patient covariate containing MIC.
#' @param mic_max_age Maximum age in hours of MIC evidence.
#' @param safety_auc_upper Optional absolute AUC safety ceiling.
#' @param unit Display unit.
#' @param status Governance status.
#' @param metadata Additional metadata.
#' @export
lator_endpoint_vancomycin <- function(
    drug = "vancomycin", lower, upper, source, mic_variable = "MIC",
    safety_auc_upper = NULL, unit = "AUC24/MIC", mic_max_age = 168,
    status = c("draft", "reviewed", "qualified"), metadata = list()) {
  lator_endpoint(
    id = paste0("vancomycin-aucmic-", tolower(gsub("[^A-Za-z0-9]+", "-", drug))),
    name = paste(drug, "AUC/MIC target"), drug = drug,
    kind = "auc_mic_range", metric = "AUC24/MIC", unit = unit,
    rules = list(
      lower = lower, upper = upper, mic_variable = mic_variable,
      safety_auc_upper = safety_auc_upper, mic_max_age = mic_max_age
    ),
    source = source, status = match.arg(status),
    metadata = c(list(domain = "glycopeptide", target_policy = "efficacy-and-safety"), metadata)
  )
}

#' Aminoglycoside exposure endpoint
#' @param drug Drug name.
#' @param efficacy_lower Required lower Cmax/MIC or AUC/MIC ratio.
#' @param trough_upper Trough safety ceiling.
#' @param source Evidence or policy provenance.
#' @param efficacy_upper Optional upper efficacy-exposure bound.
#' @param efficacy_metric `"Cmax/MIC"` or `"AUC/MIC"`.
#' @param mic_variable Patient covariate containing MIC.
#' @param mic_max_age Maximum age in hours of MIC evidence.
#' @param unit Display unit for the efficacy ratio.
#' @param status Governance status.
#' @param metadata Additional metadata.
#' @export
lator_endpoint_aminoglycoside <- function(
    drug, efficacy_lower, trough_upper, source, efficacy_upper = NULL,
    efficacy_metric = c("Cmax/MIC", "AUC/MIC"), mic_variable = "MIC",
    unit = "ratio", mic_max_age = 168,
    status = c("draft", "reviewed", "qualified"),
    metadata = list()) {
  efficacy_metric <- match.arg(efficacy_metric)
  lator_endpoint(
    id = paste0("aminoglycoside-", tolower(gsub("[^A-Za-z0-9]+", "-", drug))),
    name = paste(drug, efficacy_metric, "with trough safety"), drug = drug,
    kind = "peak_mic_safety", metric = efficacy_metric, unit = unit,
    rules = list(
      efficacy_lower = efficacy_lower, efficacy_upper = efficacy_upper,
      efficacy_metric = efficacy_metric, trough_upper = trough_upper,
      mic_variable = mic_variable, mic_max_age = mic_max_age
    ),
    source = source, status = match.arg(status),
    metadata = c(list(domain = "aminoglycoside", target_policy = "composite"), metadata)
  )
}

#' Tacrolimus trough endpoint
#' @param lower,upper Predose concentration bounds.
#' @param unit Concentration unit.
#' @param source Evidence or policy provenance.
#' @param drug Drug name.
#' @param status Governance status.
#' @param metadata Organ, time-after-transplant, and risk-stratum metadata.
#' @export
lator_endpoint_tacrolimus <- function(
    lower, upper, unit, source, drug = "tacrolimus",
    status = c("draft", "reviewed", "qualified"), metadata = list()) {
  lator_endpoint(
    id = "tacrolimus-trough", name = "Tacrolimus predose target", drug = drug,
    kind = "trough_range", metric = "C0", unit = unit,
    rules = list(lower = lower, upper = upper),
    source = source, status = match.arg(status),
    metadata = c(list(domain = "transplant-immunosuppression"), metadata)
  )
}

#' Mycophenolic-acid AUC endpoint
#' @param lower,upper AUC bounds over the configured interval.
#' @param unit AUC unit.
#' @param source Evidence or policy provenance.
#' @param drug Drug/analyte name.
#' @param status Governance status.
#' @param metadata Additional metadata.
#' @export
lator_endpoint_mycophenolate <- function(
    lower, upper, unit, source, drug = "mycophenolic acid",
    status = c("draft", "reviewed", "qualified"), metadata = list()) {
  lator_endpoint(
    id = "mycophenolic-acid-auc", name = "Mycophenolic acid AUC target",
    drug = drug, kind = "auc_range", metric = "AUC0-tau", unit = unit,
    rules = list(lower = lower, upper = upper, scale = 1),
    source = source, status = match.arg(status),
    metadata = c(list(domain = "transplant-immunosuppression"), metadata)
  )
}

#' Busulfan cumulative-AUC endpoint
#' @param lower,upper Cumulative AUC bounds.
#' @param unit AUC unit.
#' @param source Evidence or policy provenance.
#' @param doses Number of equivalent dose intervals represented by the target.
#' @param drug Drug name.
#' @param status Governance status.
#' @param metadata Additional metadata.
#' @export
lator_endpoint_busulfan <- function(
    lower, upper, unit, source, doses = 1L, drug = "busulfan",
    status = c("draft", "reviewed", "qualified"), metadata = list()) {
  doses <- .lator_number(doses, "doses", positive = TRUE)
  lator_endpoint(
    id = "busulfan-cumulative-auc", name = "Busulfan cumulative AUC target",
    drug = drug, kind = "auc_range", metric = "cumulative AUC", unit = unit,
    rules = list(lower = lower, upper = upper, scale = doses),
    source = source, status = match.arg(status),
    metadata = c(list(domain = "conditioning", equivalent_intervals = doses), metadata)
  )
}

#' High-dose methotrexate timed-concentration endpoint
#' @param targets Data frame with `hours_after_dose`, `upper`, and optional
#'   `lower` and `tolerance` columns.
#' @param unit Concentration unit.
#' @param source Evidence or policy provenance.
#' @param drug Drug name.
#' @param default_tolerance Default nearest-prediction tolerance in hours.
#' @param status Governance status.
#' @param metadata Additional metadata.
#' @export
lator_endpoint_methotrexate <- function(
    targets, unit, source, drug = "methotrexate", default_tolerance = 1,
    status = c("draft", "reviewed", "qualified"), metadata = list()) {
  targets <- as.data.frame(targets)
  if (!"lower" %in% names(targets)) targets$lower <- -Inf
  if (!"tolerance" %in% names(targets)) targets$tolerance <- default_tolerance
  lator_endpoint(
    id = "methotrexate-timed-clearance",
    name = "High-dose methotrexate timed clearance", drug = drug,
    kind = "timed_thresholds", metric = "timed concentration", unit = unit,
    rules = list(targets = targets, anchor = "last_dose"),
    source = source, status = match.arg(status),
    metadata = c(list(domain = "high-dose-methotrexate", target_policy = "timed-safety"), metadata)
  )
}

#' Warfarin INR time-in-range endpoint
#' @param lower,upper INR bounds.
#' @param target_fraction Required fraction of the forecast interval in range.
#' @param source Evidence or institutional-policy provenance.
#' @param drug Intervention name.
#' @param status Governance status.
#' @param metadata Additional metadata.
#' @export
lator_endpoint_warfarin <- function(
    lower, upper, target_fraction, source, drug = "warfarin",
    status = c("draft", "reviewed", "qualified"), metadata = list()) {
  lator_endpoint(
    id = "warfarin-inr-time-in-range", name = "Warfarin INR time in range",
    drug = drug, kind = "time_in_range", metric = "INR TTR", unit = "fraction",
    rules = list(
      lower = lower, upper = upper, target_fraction = target_fraction
    ),
    source = source, status = match.arg(status),
    metadata = c(list(domain = "anticoagulation", prediction_endpoint = "INR"), metadata)
  )
}

.lator_trapz <- function(time, value) {
  keep <- is.finite(time) & is.finite(value)
  time <- time[keep]; value <- value[keep]
  if (length(time) < 2L) return(NA_real_)
  ordering <- order(time); time <- time[ordering]; value <- value[ordering]
  sum(diff(time) * (value[-length(value)] + value[-1L]) / 2)
}

.lator_nca_auc <- function(time, value) {
  profile <- data.frame(ID = 1L, TIME = as.numeric(time), CONC = as.numeric(value))
  result <- tryCatch(
    LibeRation::nm_nca(
      profile, time = "TIME", concentration = "CONC", id = "ID",
      method = "lin_up_log_down", engine = "native"
    )$results$AUCLAST[[1L]],
    error = function(e) NA_real_
  )
  if (is.finite(result)) result else .lator_trapz(time, value)
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

.lator_time_in_range <- function(time, value, lower, upper) {
  lower_line <- rep(lower, length(time))
  upper_line <- rep(upper, length(time))
  max(0, min(1,
    .lator_time_above(time, value, lower_line) -
      .lator_time_above(time, value, upper_line)
  ))
}

.lator_endpoint_covariate <- function(
    patient, name, times, method = "locf", max_age,
    freshness_override = NULL) {
  if (is.null(patient)) .lator_stop("This endpoint requires a patient timeline.")
  resolved <- lator_covariate_at(
    patient, name, times, method = method, max_age = max_age
  )
  if (any(!is.finite(resolved$value))) {
    stale <- resolved$status %in% "stale"
    if (is.null(freshness_override) || !any(stale)) {
      .lator_stop(
        "Required endpoint covariate `", name, "` is unresolved or older than ",
        format(max_age), " hours. Add current evidence or supply an explicit ",
        "audited `freshness_override`."
      )
    }
    if (!is.list(freshness_override)) {
      .lator_stop("`freshness_override` must be a named audit list.")
    }
    actor <- trimws(as.character(freshness_override$actor %||% "")[[1L]])
    reason <- trimws(as.character(freshness_override$reason %||% "")[[1L]])
    if (!nzchar(actor) || !nzchar(reason)) {
      .lator_stop("A freshness override requires non-empty `actor` and `reason` fields.")
    }
    overridden <- lator_covariate_at(
      patient, name, times, method = method, max_age = Inf
    )
    if (any(!is.finite(overridden$value))) {
      .lator_stop("Required endpoint covariate `", name, "` remains unresolved after override.")
    }
    resolved <- overridden
    attr(resolved, "freshness_override") <- list(
      actor = actor, reason = reason,
      recorded_at = .lator_now(), configured_max_age = max_age,
      maximum_source_age = max(resolved$age, na.rm = TRUE),
      source_event_ids = unique(resolved$source_event_id[nzchar(resolved$source_event_id)])
    )
  }
  values <- as.numeric(resolved$value)
  attr(values, "freshness_evidence") <- resolved
  values
}

.lator_prediction_columns <- function(predictions, value_column = NULL) {
  time <- intersect(c("TIME", "time"), names(predictions))[1L]
  if (!is.null(value_column)) {
    value_column <- as.character(value_column)[[1L]]
    if (!value_column %in% names(predictions)) {
      .lator_stop("Requested endpoint prediction column `", value_column, "` is unavailable.")
    }
    value <- value_column
  } else {
    value <- intersect(c("IPRED", "PRED", "DV", "value", "concentration"), names(predictions))[1L]
  }
  if (is.na(time) || is.na(value)) .lator_stop("Predictions require TIME/time and IPRED/PRED/value columns.")
  list(time = time, value = value)
}

.lator_endpoint_combine_evaluations <- function(endpoint, evaluations) {
  endpoint <- lator_endpoint_validate(endpoint)
  components <- endpoint$rules$components
  if (length(evaluations) != length(components)) {
    .lator_stop("Multi-endpoint evaluation count does not match its components.")
  }
  sim_ids <- lapply(evaluations, function(item) {
    as.character(item$results$SIM)
  })
  common <- Reduce(intersect, sim_ids)
  if (!length(common)) {
    .lator_stop("Multi-endpoint components have no common conditional ETA draws.")
  }
  component_ids <- vapply(components, `[[`, character(1), "component_id")
  weights <- vapply(components, `[[`, numeric(1), "weight")
  weights <- weights / sum(weights)
  attained <- score <- metric <- matrix(
    NA_real_, nrow = length(common), ncol = length(components),
    dimnames = list(common, component_ids)
  )
  long <- vector("list", length(components))
  component_summary <- vector("list", length(components))
  for (index in seq_along(components)) {
    evaluation <- evaluations[[index]]
    rows <- evaluation$results[
      match(common, as.character(evaluation$results$SIM)), , drop = FALSE
    ]
    attained[, index] <- as.numeric(as.logical(rows$attained))
    score[, index] <- suppressWarnings(as.numeric(rows$score))
    metric[, index] <- suppressWarnings(as.numeric(rows$metric))
    utility <- exp(-pmax(score[, index], 0))
    utility[!is.finite(utility)] <- 0
    component <- components[[index]]
    minimum <- component$minimum_attainment
    probability <- mean(as.logical(attained[, index]), na.rm = TRUE)
    component_summary[[index]] <- data.frame(
      component_id = component$component_id,
      endpoint_id = component$endpoint$id,
      endpoint_version = component$endpoint$version,
      name = component$endpoint$name,
      role = component$role,
      hard_constraint = component$hard_constraint,
      minimum_attainment = if (component$hard_constraint) minimum else NA_real_,
      weight = component$weight,
      normalized_weight = weights[[index]],
      attainment_probability = probability,
      median_metric = stats::median(metric[, index], na.rm = TRUE),
      expected_utility = mean(utility, na.rm = TRUE),
      constraint_pass = !component$hard_constraint ||
        (is.finite(probability) && probability >= minimum),
      stringsAsFactors = FALSE
    )
    long[[index]] <- data.frame(
      SIM = common, component_id = component$component_id,
      endpoint_id = component$endpoint$id,
      role = component$role, metric = metric[, index],
      attained = as.logical(attained[, index]), score = score[, index],
      utility = utility, stringsAsFactors = FALSE
    )
  }
  utilities <- exp(-pmax(score, 0))
  utilities[!is.finite(utilities)] <- 0
  draw_utility <- drop(utilities %*% weights)
  joint <- apply(attained, 1L, function(value) all(as.logical(value)))
  primary <- which(vapply(
    components, function(component) identical(component$role, "primary"),
    logical(1)
  ))
  hard <- which(vapply(
    components, function(component) isTRUE(component$hard_constraint),
    logical(1)
  ))
  hard_draw <- if (length(hard)) {
    apply(attained[, hard, drop = FALSE], 1L, function(value) {
      all(as.logical(value))
    })
  } else rep(TRUE, length(common))
  component_summary <- do.call(rbind, component_summary)
  per_draw <- data.frame(
    SIM = common, metric = draw_utility, attained = joint,
    score = 1 - draw_utility, utility = draw_utility,
    primary_attained = as.logical(attained[, primary]),
    hard_constraints_attained = hard_draw,
    stringsAsFactors = FALSE
  )
  list(
    endpoint_id = endpoint$id, endpoint_version = endpoint$version,
    results = per_draw,
    attainment_probability = mean(joint, na.rm = TRUE),
    joint_attainment_probability = mean(joint, na.rm = TRUE),
    primary_attainment_probability = mean(
      as.logical(attained[, primary]), na.rm = TRUE
    ),
    hard_constraints_pass = all(component_summary$constraint_pass),
    expected_utility = mean(draw_utility, na.rm = TRUE),
    median_metric = stats::median(draw_utility, na.rm = TRUE),
    median_score = stats::median(1 - draw_utility, na.rm = TRUE),
    components = component_summary,
    component_results = do.call(rbind, long),
    component_evaluations = stats::setNames(evaluations, component_ids),
    covariate_freshness = unname(unlist(lapply(
      evaluations, `[[`, "covariate_freshness"
    ), recursive = FALSE)),
    utility_definition = list(
      transform = "exp(-normalized_loss)",
      aggregation = "weighted_arithmetic_mean",
      normalized_weights = stats::setNames(as.list(weights), component_ids)
    ),
    evaluated_at = .lator_now()
  )
}

.lator_endpoint_target_text <- function(endpoint) {
  endpoint <- lator_endpoint_validate(endpoint)
  unit <- trimws(as.character(endpoint$unit %||% ""))
  with_unit <- function(text) {
    paste0(text, if (nzchar(unit)) paste0(" ", unit) else "")
  }
  number <- function(value) {
    format(
      as.numeric(value), trim = TRUE, scientific = FALSE,
      digits = 6
    )
  }
  range_text <- function(lower, upper) {
    with_unit(paste0(number(lower), " \u2013 ", number(upper)))
  }
  if (endpoint$kind %in%
      c("therapeutic_range", "trough_range", "auc_range")) {
    return(range_text(endpoint$rules$lower, endpoint$rules$upper))
  }
  if (endpoint$kind == "auc_mic_range") {
    result <- range_text(endpoint$rules$lower, endpoint$rules$upper)
    if (!is.null(endpoint$rules$safety_auc_upper)) {
      result <- paste0(
        result, "; absolute AUC \u2264 ",
        number(endpoint$rules$safety_auc_upper)
      )
    }
    return(result)
  }
  if (endpoint$kind == "peak_mic_safety") {
    result <- paste0(
      endpoint$rules$efficacy_metric, " \u2265 ",
      number(endpoint$rules$efficacy_lower)
    )
    if (!is.null(endpoint$rules$efficacy_upper)) {
      result <- paste0(
        result, " and \u2264 ", number(endpoint$rules$efficacy_upper)
      )
    }
    return(paste0(
      result, "; trough \u2264 ", number(endpoint$rules$trough_upper)
    ))
  }
  if (endpoint$kind == "fraction_time_above_threshold") {
    return(paste0(
      "\u2265 ", number(100 * endpoint$rules$target_fraction),
      "% of interval above ",
      number(endpoint$rules$threshold_multiplier), "\u00d7 MIC"
    ))
  }
  if (endpoint$kind == "time_in_range") {
    return(paste0(
      "\u2265 ", number(100 * endpoint$rules$target_fraction),
      "% within ", number(endpoint$rules$lower), " \u2013 ",
      number(endpoint$rules$upper)
    ))
  }
  if (endpoint$kind == "pre_event_target") {
    return(paste(
      nrow(endpoint$rules$targets), "protocol-defined pre-event window(s)"
    ))
  }
  if (endpoint$kind == "timed_thresholds") {
    return(paste(
      nrow(endpoint$rules$targets), "protocol-defined timed threshold(s)"
    ))
  }
  "Versioned custom target"
}

.lator_endpoint_outcome_summary <- function(
    endpoint, evaluation, probs = c(0.05, 0.5, 0.95)) {
  endpoint <- lator_endpoint_validate(endpoint)
  probs <- as.numeric(probs)
  if (length(probs) != 3L || any(!is.finite(probs)) ||
      any(probs <= 0 | probs >= 1) || any(diff(probs) <= 0)) {
    .lator_stop(
      "`probs` must contain three increasing probabilities between zero and one."
    )
  }
  one <- function(definition, item, component = NULL) {
    metrics <- suppressWarnings(as.numeric(item$results$metric))
    metrics <- metrics[is.finite(metrics)]
    quantiles <- if (length(metrics)) {
      stats::quantile(
        metrics, probs = probs, names = FALSE, type = 8, na.rm = TRUE
      )
    } else rep(NA_real_, 3L)
    fraction <- definition$kind %in%
      c("fraction_time_above_threshold", "time_in_range")
    scale <- if (fraction) 100 else 1
    component <- component %||% list()
    probability <- as.numeric(
      component$attainment_probability %||%
        item$attainment_probability %||% NA_real_
    )
    hard <- isTRUE(component$hard_constraint)
    data.frame(
      component_id = as.character(
        component$component_id %||%
          paste(definition$id, definition$version, sep = "@")
      ),
      endpoint_id = definition$id,
      endpoint_version = definition$version,
      name = definition$name,
      role = as.character(component$role %||% "primary"),
      metric = definition$metric,
      display_unit = if (fraction) "%" else definition$unit,
      display_scale = scale,
      lower = unname(quantiles[[1L]]) * scale,
      median = unname(quantiles[[2L]]) * scale,
      upper = unname(quantiles[[3L]]) * scale,
      lower_probability = probs[[1L]],
      upper_probability = probs[[3L]],
      target = .lator_endpoint_target_text(definition),
      attainment_probability = probability,
      hard_constraint = hard,
      minimum_attainment = if (hard) {
        as.numeric(component$minimum_attainment)
      } else NA_real_,
      constraint_pass = if (hard) {
        isTRUE(component$constraint_pass)
      } else TRUE,
      weight = as.numeric(component$weight %||% 1),
      normalized_weight = as.numeric(component$normalized_weight %||% 1),
      stringsAsFactors = FALSE
    )
  }
  if (!identical(endpoint$kind, "multi_endpoint")) {
    return(one(endpoint, evaluation))
  }
  summaries <- evaluation$components
  if (!is.data.frame(summaries) || nrow(summaries) !=
      length(endpoint$rules$components)) {
    .lator_stop("Multi-endpoint evaluation component summaries are unavailable.")
  }
  do.call(rbind, lapply(seq_along(endpoint$rules$components), function(index) {
    component <- endpoint$rules$components[[index]]
    component_id <- component$component_id
    component_summary <- as.list(summaries[
      match(component_id, summaries$component_id), , drop = FALSE
    ])
    item <- evaluation$component_evaluations[[component_id]] %||% NULL
    if (is.null(item)) {
      rows <- evaluation$component_results[
        evaluation$component_results$component_id == component_id,
        , drop = FALSE
      ]
      item <- list(
        results = rows,
        attainment_probability = component_summary$attainment_probability
      )
    }
    one(component$endpoint, item, component_summary)
  }))
}

#' Evaluate predictions against a therapeutic endpoint
#' @param endpoint Endpoint definition.
#' @param predictions Prediction data frame. Optional `SIM` identifies uncertainty replicates.
#' @param patient Patient timeline, required when an endpoint obtains MIC or an event anchor from it.
#' @param interval Optional two-element evaluation interval.
#' @param value_column Optional explicit prediction column. Regimen simulation
#'   uses `"DV"` when residual observation variability is requested and
#'   `"IPRED"` otherwise, preventing a residualised run from being silently
#'   scored on its non-residual individual prediction.
#' @param freshness_override Optional named list with `actor` and `reason`.
#'   This permits stale endpoint covariate evidence for one evaluation and is
#'   retained in the returned audit record; it never changes the endpoint.
#' @return Per-replicate metrics and an aggregate target-attainment summary.
#' @export
lator_endpoint_evaluate <- function(endpoint, predictions, patient = NULL,
                                    interval = NULL, value_column = NULL,
                                    freshness_override = NULL) {
  endpoint <- lator_endpoint_validate(endpoint)
  if (identical(endpoint$kind, "multi_endpoint")) {
    evaluations <- lapply(endpoint$rules$components, function(component) {
      lator_endpoint_evaluate(
        component$endpoint, predictions, patient = patient,
        interval = interval, value_column = value_column,
        freshness_override = freshness_override
      )
    })
    return(.lator_endpoint_combine_evaluations(endpoint, evaluations))
  }
  predictions <- as.data.frame(predictions)
  columns <- .lator_prediction_columns(predictions, value_column)
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
  freshness_evidence <- list()
  resolve_endpoint_covariate <- function(name, times) {
    values <- .lator_endpoint_covariate(
      patient, name, times, method = "locf",
      max_age = endpoint$rules$mic_max_age,
      freshness_override = freshness_override
    )
    evidence <- attr(values, "freshness_evidence")
    key <- paste(name, min(times), max(times), sep = "|")
    freshness_evidence[[key]] <<- list(
      covariate = name,
      configured_max_age = endpoint$rules$mic_max_age,
      source_event_ids = unique(evidence$source_event_id[nzchar(evidence$source_event_id)]),
      maximum_source_age = max(evidence$age, na.rm = TRUE),
      override = attr(evidence, "freshness_override") %||% NULL
    )
    values
  }

  evaluate_one <- function(data) {
    time <- data$.lator_time; value <- data$.lator_value
    if (endpoint$kind == "therapeutic_range") {
      metric <- if (identical(endpoint$metric, "trough")) min(value, na.rm = TRUE) else {
        duration <- diff(range(time))
        if (duration > 0) .lator_nca_auc(time, value) / duration else utils::tail(value, 1L)
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
      metric <- .lator_nca_auc(time, value) * as.numeric(endpoint$rules$scale %||% 1)
      lower <- endpoint$rules$lower; upper <- endpoint$rules$upper
      return(c(metric = metric, attained = metric >= lower && metric <= upper,
               score = abs(metric - mean(c(lower, upper))) / (upper - lower)))
    }
    if (endpoint$kind == "auc_mic_range") {
      auc <- .lator_nca_auc(time, value)
      mic <- resolve_endpoint_covariate(endpoint$rules$mic_variable, time)
      metric <- .lator_nca_auc(time, value / mic)
      lower <- endpoint$rules$lower; upper <- endpoint$rules$upper
      safety <- endpoint$rules$safety_auc_upper
      attained <- metric >= lower && metric <= upper &&
        (is.null(safety) || auc <= safety)
      penalty <- abs(metric - mean(c(lower, upper))) / (upper - lower)
      if (!is.null(safety) && auc > safety) penalty <- penalty + (auc - safety) / safety
      return(c(metric = metric, attained = attained, score = penalty))
    }
    if (endpoint$kind == "peak_mic_safety") {
      mic <- resolve_endpoint_covariate(endpoint$rules$mic_variable, time)
      efficacy <- if (identical(endpoint$rules$efficacy_metric, "AUC/MIC")) {
        .lator_nca_auc(time, value / mic)
      } else max(value / mic, na.rm = TRUE)
      trough <- min(value, na.rm = TRUE)
      lower <- endpoint$rules$efficacy_lower
      upper <- endpoint$rules$efficacy_upper
      attained <- efficacy >= lower &&
        (is.null(upper) || efficacy <= upper) &&
        trough <= endpoint$rules$trough_upper
      score <- max(0, lower - efficacy) / lower +
        max(0, trough - endpoint$rules$trough_upper) /
          endpoint$rules$trough_upper
      if (!is.null(upper)) score <- score + max(0, efficacy - upper) / upper
      return(c(metric = efficacy, attained = attained, score = score))
    }
    if (endpoint$kind == "fraction_time_above_threshold") {
      if (is.null(patient)) .lator_stop("A patient timeline is required to resolve MIC.")
      mic <- resolve_endpoint_covariate(endpoint$rules$mic_variable, time)
      threshold <- mic * endpoint$rules$threshold_multiplier
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
    if (endpoint$kind == "timed_thresholds") {
      targets <- endpoint$rules$targets
      anchor <- 0
      if (identical(endpoint$rules$anchor, "last_dose") && !is.null(patient)) {
        doses <- .lator_match_therapy_events(patient, "dose", endpoint$drug)
        eligible_doses <- Filter(function(event) event$time <= min(time), doses)
        if (length(eligible_doses)) {
          anchor <- max(vapply(eligible_doses, `[[`, numeric(1), "time"))
        }
      }
      measured <- vapply(seq_len(nrow(targets)), function(index) {
        target_time <- anchor + targets$hours_after_dose[index]
        nearest <- which.min(abs(time - target_time))
        if (!length(nearest) ||
            abs(time[nearest] - target_time) > targets$tolerance[index]) {
          return(NA_real_)
        }
        value[nearest]
      }, numeric(1))
      attained <- all(is.finite(measured) &
                        measured >= targets$lower &
                        measured <= targets$upper)
      width <- pmax(
        ifelse(is.finite(targets$lower), targets$upper - targets$lower,
               abs(targets$upper)),
        .Machine$double.eps
      )
      score <- mean(
        pmax(0, targets$lower - measured, measured - targets$upper) / width,
        na.rm = TRUE
      )
      if (any(!is.finite(measured))) score <- Inf
      return(c(metric = mean(measured, na.rm = TRUE), attained = attained, score = score))
    }
    if (endpoint$kind == "time_in_range") {
      metric <- .lator_time_in_range(
        time, value, endpoint$rules$lower, endpoint$rules$upper
      )
      target <- endpoint$rules$target_fraction
      return(c(
        metric = metric, attained = metric >= target,
        score = max(0, target - metric)
      ))
    }
    .lator_stop("Custom endpoint evaluation requires a registered evaluator and is not executable by default.")
  }
  result <- as.data.frame(do.call(rbind, lapply(groups, evaluate_one)))
  result$SIM <- names(groups); rownames(result) <- NULL
  list(
    endpoint_id = endpoint$id, endpoint_version = endpoint$version,
    results = result, attainment_probability = mean(as.logical(result$attained)),
    median_metric = stats::median(result$metric), median_score = stats::median(result$score),
    covariate_freshness = unname(freshness_evidence),
    evaluated_at = .lator_now()
  )
}

.lator_builtin_endpoints <- function() list(
  list(id = "template-aed-range", name = "Antiseizure-medicine concentration", kind = "therapeutic_range",
       constructor = "lator_endpoint_aed",
       description = "Individual or reference concentration range with explicit clinical provenance."),
  list(id = "template-vancomycin-aucmic", name = "Vancomycin AUC24/MIC", kind = "auc_mic_range",
       constructor = "lator_endpoint_vancomycin",
       description = "Exposure/MIC efficacy range with an optional absolute AUC safety ceiling."),
  list(id = "template-betalactam-ftmic", name = "Beta-lactam fT>MIC", kind = "fraction_time_above_threshold",
       constructor = "lator_endpoint_beta_lactam",
       description = "Unbound fraction of the interval above an institution-defined MIC multiple."),
  list(id = "template-aminoglycoside", name = "Aminoglycoside efficacy and trough safety", kind = "peak_mic_safety",
       constructor = "lator_endpoint_aminoglycoside",
       description = "Cmax/MIC or AUC/MIC efficacy combined with a trough safety ceiling."),
  list(id = "template-tacrolimus", name = "Tacrolimus predose concentration", kind = "trough_range",
       constructor = "lator_endpoint_tacrolimus",
       description = "Organ-, risk-, and time-after-transplant-specific predose target."),
  list(id = "template-mycophenolate", name = "Mycophenolic-acid AUC", kind = "auc_range",
       constructor = "lator_endpoint_mycophenolate",
       description = "AUC over a configured dosing interval, including limited-sampling workflows."),
  list(id = "template-busulfan", name = "Busulfan cumulative AUC", kind = "auc_range",
       constructor = "lator_endpoint_busulfan",
       description = "Per-dose or cumulative conditioning exposure target."),
  list(id = "template-methotrexate", name = "High-dose methotrexate timed clearance", kind = "timed_thresholds",
       constructor = "lator_endpoint_methotrexate",
       description = "Protocol-specific concentrations at timed post-dose checkpoints."),
  list(id = "template-atg-pre-event", name = "Active ATG transplant exposure", kind = "pre_event_target",
       constructor = "lator_endpoint_atg",
       description = "Concentration or exposure windows relative to transplantation."),
  list(id = "template-warfarin", name = "Warfarin INR time in range", kind = "time_in_range",
       constructor = "lator_endpoint_warfarin",
       description = "Probability and duration of a predicted INR response within its target range.")
)

.lator_endpoint_template_field <- function(
    name, label, type = "text", required = TRUE, default = "",
    help = "", options = character(), span = 1L) {
  list(
    name = name, label = label, type = type, required = isTRUE(required),
    default = default, help = help, options = as.character(options),
    span = as.integer(span)
  )
}

.lator_endpoint_template_specs <- function() {
  field <- .lator_endpoint_template_field
  common <- function(source_help = "Citation or institutional protocol used for this definition.") {
    list(
      field("source", "Evidence or policy source", required = TRUE,
            help = source_help, span = 2L),
      field("status", "Governance status", type = "select", default = "draft",
            options = c("draft", "reviewed"),
            help = "Qualified status requires a separately attested governance record."),
      field("version", "Endpoint version", default = "1.0.0")
    )
  }
  c(list(
    "template-aed-range" = c(list(
      field("drug", "Drug or analyte"),
      field("unit", "Concentration unit", default = "mg/L"),
      field("lower", "Lower target", type = "number"),
      field("upper", "Upper target", type = "number")
    ), common()),
    "template-vancomycin-aucmic" = c(list(
      field("drug", "Drug", default = "vancomycin"),
      field("unit", "Display unit", default = "AUC24/MIC"),
      field("lower", "Lower AUC/MIC target", type = "number"),
      field("upper", "Upper AUC/MIC target", type = "number"),
      field("safety_auc_upper", "Optional absolute AUC ceiling",
            type = "number", required = FALSE),
      field("mic_variable", "MIC covariate", default = "MIC"),
      field("mic_max_age", "Maximum MIC age (hours)", type = "number",
            default = 168,
            help = "Older MIC evidence is rejected unless an audited override is supplied.")
    ), common()),
    "template-betalactam-ftmic" = c(list(
      field("drug", "Drug"),
      field("target_fraction", "Required interval fraction", type = "number",
            default = 0.4),
      field("mic_variable", "MIC covariate", default = "MIC"),
      field("mic_max_age", "Maximum MIC age (hours)", type = "number",
            default = 168,
            help = "Older MIC evidence is rejected unless an audited override is supplied."),
      field("threshold_multiplier", "MIC multiplier", type = "number",
            default = 1),
      field("free_fraction", "Unbound fraction", type = "number", default = 1)
    ), common()),
    "template-aminoglycoside" = c(list(
      field("drug", "Drug"),
      field("efficacy_metric", "Efficacy metric", type = "select",
            default = "Cmax/MIC", options = c("Cmax/MIC", "AUC/MIC")),
      field("efficacy_lower", "Minimum efficacy ratio", type = "number"),
      field("efficacy_upper", "Optional maximum efficacy ratio",
            type = "number", required = FALSE),
      field("trough_upper", "Trough safety ceiling", type = "number"),
      field("mic_variable", "MIC covariate", default = "MIC"),
      field("mic_max_age", "Maximum MIC age (hours)", type = "number",
            default = 168,
            help = "Older MIC evidence is rejected unless an audited override is supplied."),
      field("unit", "Display unit", default = "ratio")
    ), common()),
    "template-tacrolimus" = c(list(
      field("drug", "Drug", default = "tacrolimus"),
      field("unit", "Concentration unit", default = "ng/mL"),
      field("lower", "Lower trough target", type = "number"),
      field("upper", "Upper trough target", type = "number")
    ), common()),
    "template-mycophenolate" = c(list(
      field("drug", "Drug or analyte", default = "mycophenolic acid"),
      field("unit", "AUC unit", default = "mg*h/L"),
      field("lower", "Lower AUC target", type = "number"),
      field("upper", "Upper AUC target", type = "number")
    ), common()),
    "template-busulfan" = c(list(
      field("drug", "Drug", default = "busulfan"),
      field("unit", "Cumulative AUC unit", default = "mg*h/L"),
      field("lower", "Lower cumulative AUC", type = "number"),
      field("upper", "Upper cumulative AUC", type = "number"),
      field("doses", "Equivalent dose intervals", type = "number", default = 1)
    ), common()),
    "template-methotrexate" = c(list(
      field("drug", "Drug", default = "methotrexate"),
      field("unit", "Concentration unit", default = "micromol/L"),
      field(
        "targets", "Timed thresholds", type = "textarea",
        default = "24,-Inf,10,1\n48,-Inf,1,1",
        help = "One row per checkpoint: hours after dose, lower, upper, tolerance (hours).",
        span = 2L
      )
    ), common()),
    "template-atg-pre-event" = c(list(
      field("drug", "ATG product", default = "ATG"),
      field("unit", "Concentration or exposure unit"),
      field("anchor", "Procedure-event anchor", default = "transplantation"),
      field(
        "targets", "Target windows", type = "textarea",
        default = "-24,-18,1,3\n-6,0,2,4",
        help = "One row per window: start, end, lower, upper; times are relative to the anchor.",
        span = 2L
      )
    ), common()),
    "template-warfarin" = c(list(
      field("drug", "Intervention", default = "warfarin"),
      field("lower", "Lower INR bound", type = "number", default = 2),
      field("upper", "Upper INR bound", type = "number", default = 3),
      field("target_fraction", "Required time in range", type = "number",
            default = 0.7)
    ), common())
  ))
}

.lator_endpoint_presets <- function() {
  asm_source <- paste(
    "Reference starting range from Reimers et al. (2018),",
    "Drug Design, Development and Therapy 12:271-280,",
    "doi:10.2147/DDDT.S154388. Drug-fasting steady-state serum range;",
    "confirm the individual target, assay and institutional policy."
  )
  list(
    list(
      id = "preset-carbamazepine", aliases = c("carbamazepine"),
      therapeutic_class = "antiseizure",
      template_id = "template-aed-range",
      label = "Carbamazepine reference serum range",
      values = list(lower = 4, upper = 11, unit = "mg/L", source = asm_source)
    ),
    list(
      id = "preset-lamotrigine", aliases = c("lamotrigine"),
      therapeutic_class = "antiseizure",
      template_id = "template-aed-range",
      label = "Lamotrigine reference serum range",
      values = list(lower = 3, upper = 13, unit = "mg/L", source = asm_source)
    ),
    list(
      id = "preset-levetiracetam", aliases = c("levetiracetam"),
      therapeutic_class = "antiseizure",
      template_id = "template-aed-range",
      label = "Levetiracetam reference serum range",
      values = list(lower = 5, upper = 41, unit = "mg/L", source = asm_source)
    ),
    list(
      id = "preset-phenobarbital",
      aliases = c("phenobarbital", "phenobarbitone"),
      therapeutic_class = "antiseizure",
      template_id = "template-aed-range",
      label = "Phenobarbital reference serum range",
      values = list(lower = 12, upper = 30, unit = "mg/L", source = asm_source)
    ),
    list(
      id = "preset-phenytoin", aliases = c("phenytoin"),
      therapeutic_class = "antiseizure",
      template_id = "template-aed-range",
      label = "Total phenytoin reference serum range",
      values = list(lower = 10, upper = 20, unit = "mg/L", source = asm_source)
    ),
    list(
      id = "preset-valproate",
      aliases = c("valproate", "valproic acid", "sodium valproate"),
      therapeutic_class = "antiseizure",
      template_id = "template-aed-range",
      label = "Valproate reference serum range",
      values = list(lower = 43, upper = 101, unit = "mg/L", source = asm_source)
    ),
    list(
      id = "preset-warfarin-af", aliases = c("warfarin"),
      therapeutic_class = "vitamin-k-antagonist",
      template_id = "template-warfarin",
      label = "Warfarin atrial-fibrillation reference target",
      values = list(
        lower = 2, upper = 3, target_fraction = 0.65,
        source = paste(
          "Editable reference starting point: target INR 2.0-3.0 and",
          "TTR review threshold 65% from NICE NG196 sections 1.6.10-1.6.11;",
          "confirm indication-specific institutional policy."
        )
      )
    ),
    list(
      id = "preset-vancomycin-mrsa", aliases = c("vancomycin"),
      therapeutic_class = "glycopeptide",
      template_id = "template-vancomycin-aucmic",
      label = "Vancomycin serious-MRSA reference target",
      values = list(
        lower = 400, upper = 600, mic_variable = "MIC",
        unit = "AUC24/MIC",
        source = paste(
          "Editable reference starting point from the 2020",
          "ASHP/PIDS/SIDP/IDSA vancomycin consensus guideline",
          "(doi:10.1093/ajhp/zxaa036); serious MRSA infections, assumed",
          "broth-microdilution MIC 1 mg/L. Do not extrapolate without review."
        )
      )
    )
  )
}

.lator_endpoint_preset_for_drug <- function(drug, therapeutic_class = "") {
  if (is.null(drug) || !length(drug) || is.na(drug[[1L]]) ||
      !nzchar(trimws(as.character(drug[[1L]])))) return(NULL)
  normalized <- tolower(trimws(as.character(drug[[1L]])))
  presets <- .lator_endpoint_presets()
  matched <- Filter(function(preset) {
    normalized %in% tolower(preset$aliases)
  }, presets)
  if (!length(matched) && nzchar(trimws(as.character(therapeutic_class %||% "")))) {
    class_name <- tolower(trimws(as.character(therapeutic_class)))
    matched <- Filter(function(preset) {
      identical(tolower(preset$therapeutic_class), class_name)
    }, presets)
    # A class match can identify the endpoint family, but it must never borrow
    # exact drug-specific targets. Those are only prefilled for an exact drug
    # alias match.
    templates <- unique(vapply(matched, `[[`, character(1), "template_id"))
    if (length(templates) == 1L) {
      matched <- list(list(
        id = paste0("class-", class_name),
        aliases = normalized, therapeutic_class = class_name,
        template_id = templates[[1L]],
        label = paste("Suggested", class_name, "endpoint family"),
        values = list()
      ))
    } else {
      matched <- list()
    }
  }
  if (length(matched)) matched[[1L]] else NULL
}

.lator_endpoint_templates_for_gui <- function(drug = NULL,
                                               therapeutic_class = "") {
  templates <- .lator_builtin_endpoints()
  specifications <- .lator_endpoint_template_specs()
  preset <- .lator_endpoint_preset_for_drug(drug, therapeutic_class)
  unname(lapply(templates, function(template) {
    template$fields <- specifications[[template$id]] %||% list()
    template$recommended <- !is.null(preset) &&
      identical(template$id, preset$template_id)
    template$preset_label <- if (isTRUE(template$recommended)) {
      preset$label
    } else ""
    if (isTRUE(template$recommended)) {
      defaults <- c(list(drug = as.character(drug)), preset$values)
      template$fields <- lapply(template$fields, function(field) {
        if (field$name %in% names(defaults)) {
          field$default <- defaults[[field$name]]
        }
        field
      })
    }
    template
  }))
}

.lator_endpoint_template_id <- function(endpoint) {
  endpoint <- lator_endpoint_validate(endpoint)
  known <- names(.lator_endpoint_template_specs())
  explicit <- as.character(endpoint$metadata$template_id %||% "")
  if (length(explicit) == 1L && explicit %in% known) return(explicit)
  switch(endpoint$kind,
    "therapeutic_range" = "template-aed-range",
    "auc_mic_range" = "template-vancomycin-aucmic",
    "fraction_time_above_threshold" = "template-betalactam-ftmic",
    "peak_mic_safety" = "template-aminoglycoside",
    "trough_range" = "template-tacrolimus",
    "timed_thresholds" = "template-methotrexate",
    "pre_event_target" = "template-atg-pre-event",
    "time_in_range" = "template-warfarin",
    "auc_range" = {
      domain <- tolower(as.character(endpoint$metadata$domain %||% ""))
      if (identical(domain, "conditioning") ||
          startsWith(tolower(endpoint$id), "busulfan")) {
        "template-busulfan"
      } else {
        "template-mycophenolate"
      }
    },
    NULL
  )
}

.lator_endpoint_rows_text <- function(values, columns) {
  values <- as.data.frame(values)
  if (!nrow(values) || !all(columns %in% names(values))) return("")
  paste(vapply(seq_len(nrow(values)), function(index) {
    paste(vapply(columns, function(column) {
      as.character(values[[column]][[index]])
    }, character(1)), collapse = ",")
  }, character(1)), collapse = "\n")
}

.lator_endpoint_edit_values <- function(endpoint) {
  endpoint <- lator_endpoint_validate(endpoint)
  template_id <- .lator_endpoint_template_id(endpoint)
  if (is.null(template_id)) return(NULL)
  values <- list(
    drug = endpoint$drug, unit = endpoint$unit, source = endpoint$source,
    status = endpoint$status, version = endpoint$version
  )
  rules <- endpoint$rules
  values <- c(values, switch(template_id,
    "template-aed-range" = list(
      lower = rules$lower, upper = rules$upper
    ),
    "template-vancomycin-aucmic" = list(
      lower = rules$lower, upper = rules$upper,
      mic_variable = rules$mic_variable,
      safety_auc_upper = rules$safety_auc_upper %||% "",
      mic_max_age = rules$mic_max_age
    ),
    "template-betalactam-ftmic" = list(
      target_fraction = rules$target_fraction,
      mic_variable = rules$mic_variable,
      mic_max_age = rules$mic_max_age,
      threshold_multiplier = rules$threshold_multiplier,
      free_fraction = rules$free_fraction
    ),
    "template-aminoglycoside" = list(
      efficacy_metric = rules$efficacy_metric,
      efficacy_lower = rules$efficacy_lower,
      efficacy_upper = rules$efficacy_upper %||% "",
      trough_upper = rules$trough_upper,
      mic_variable = rules$mic_variable,
      mic_max_age = rules$mic_max_age
    ),
    "template-tacrolimus" = list(
      lower = rules$lower, upper = rules$upper
    ),
    "template-mycophenolate" = list(
      lower = rules$lower, upper = rules$upper
    ),
    "template-busulfan" = list(
      lower = rules$lower, upper = rules$upper,
      doses = endpoint$metadata$equivalent_intervals %||%
        rules$scale %||% 1
    ),
    "template-methotrexate" = list(
      targets = .lator_endpoint_rows_text(
        rules$targets,
        c("hours_after_dose", "lower", "upper", "tolerance")
      )
    ),
    "template-atg-pre-event" = list(
      anchor = rules$anchor,
      targets = .lator_endpoint_rows_text(
        rules$targets,
        c("window_start", "window_end", "lower", "upper")
      )
    ),
    "template-warfarin" = list(
      lower = rules$lower, upper = rules$upper,
      target_fraction = rules$target_fraction
    )
  ))
  list(template_id = template_id, values = values)
}

.lator_next_endpoint_version <- function(endpoint, endpoints = list()) {
  endpoint <- lator_endpoint_validate(endpoint)
  used <- vapply(endpoints, function(candidate) {
    paste(candidate$id, candidate$version, sep = "@")
  }, character(1))
  parts <- regmatches(
    endpoint$version,
    regexec("^([0-9]+)\\.([0-9]+)\\.([0-9]+)$", endpoint$version)
  )[[1L]]
  if (length(parts) == 4L) {
    prefix <- paste(parts[[2L]], parts[[3L]], sep = ".")
    patch <- as.integer(parts[[4L]]) + 1L
    repeat {
      candidate <- paste(prefix, patch, sep = ".")
      if (!paste(endpoint$id, candidate, sep = "@") %in% used) {
        return(candidate)
      }
      patch <- patch + 1L
    }
  }
  suffix <- 1L
  repeat {
    candidate <- paste(endpoint$version, suffix, sep = ".")
    if (!paste(endpoint$id, candidate, sep = "@") %in% used) {
      return(candidate)
    }
    suffix <- suffix + 1L
  }
}

.lator_endpoint_edit_payload <- function(endpoint, endpoints = list(),
                                         original_key = "") {
  values <- .lator_endpoint_edit_values(endpoint)
  if (is.null(values)) return(NULL)
  values$values$version <- .lator_next_endpoint_version(endpoint, endpoints)
  list(
    original_key = original_key,
    original_version = endpoint$version,
    template_id = values$template_id,
    values = values$values
  )
}

.lator_endpoint_template_text <- function(values, name, default = "",
                                          required = TRUE) {
  value <- values[[name]] %||% default
  if (length(value) != 1L || is.na(value)) {
    .lator_stop("Endpoint field `", name, "` must contain one value.")
  }
  value <- trimws(as.character(value))
  if (isTRUE(required) && !nzchar(value)) {
    .lator_stop("Endpoint field `", name, "` is required.")
  }
  value
}

.lator_endpoint_template_number <- function(
    values, name, default = NULL, required = TRUE) {
  value <- values[[name]] %||% default
  if (is.null(value) || !length(value) ||
      (length(value) == 1L && !nzchar(trimws(as.character(value))))) {
    if (isTRUE(required)) {
      .lator_stop("Endpoint field `", name, "` is required.")
    }
    return(NULL)
  }
  .lator_number(value, paste0("endpoint field ", name))
}

.lator_endpoint_template_targets <- function(value, columns, label) {
  if (is.data.frame(value)) {
    if (!all(columns %in% names(value)) || !nrow(value)) {
      .lator_stop(label, " requires columns: ", paste(columns, collapse = ", "), ".")
    }
    return(value[columns])
  }
  text <- trimws(as.character(value %||% ""))
  if (length(text) != 1L || !nzchar(text)) {
    .lator_stop(label, " requires at least one target row.")
  }
  lines <- trimws(unlist(strsplit(gsub(";", "\n", text, fixed = TRUE), "\r?\n")))
  lines <- lines[nzchar(lines)]
  rows <- lapply(lines, function(line) {
    trimws(strsplit(line, ",", fixed = TRUE)[[1L]])
  })
  if (!length(rows) || any(lengths(rows) != length(columns))) {
    .lator_stop(
      label, " rows must contain ", length(columns),
      " comma-separated values: ", paste(columns, collapse = ", "), "."
    )
  }
  matrix <- do.call(rbind, rows)
  output <- as.data.frame(
    lapply(seq_along(columns), function(index) {
      suppressWarnings(as.numeric(matrix[, index]))
    }),
    stringsAsFactors = FALSE
  )
  names(output) <- columns
  if (any(is.na(as.matrix(output)))) {
    .lator_stop(label, " contains a non-numeric or missing value.")
  }
  output
}

.lator_endpoint_from_template <- function(template_id, values = list()) {
  template_id <- .lator_scalar(template_id, "template_id")
  if (!template_id %in% names(.lator_endpoint_template_specs())) {
    .lator_stop("Unknown therapeutic endpoint template: ", template_id)
  }
  if (!is.list(values)) .lator_stop("Endpoint template values must be a list.")
  text <- function(name, default = "", required = TRUE) {
    .lator_endpoint_template_text(values, name, default, required)
  }
  number <- function(name, default = NULL, required = TRUE) {
    .lator_endpoint_template_number(values, name, default, required)
  }
  source <- text("source")
  status <- match.arg(text("status", "draft"), c("draft", "reviewed", "qualified"))
  version <- text("version", "1.0.0")
  endpoint <- switch(template_id,
    "template-aed-range" = lator_endpoint_aed(
      text("drug"), number("lower"), number("upper"), text("unit"),
      source, status = status
    ),
    "template-vancomycin-aucmic" = lator_endpoint_vancomycin(
      drug = text("drug", "vancomycin"),
      lower = number("lower"), upper = number("upper"), source = source,
      mic_variable = text("mic_variable", "MIC"),
      mic_max_age = number("mic_max_age", 168),
      safety_auc_upper = number("safety_auc_upper", required = FALSE),
      unit = text("unit", "AUC24/MIC"), status = status
    ),
    "template-betalactam-ftmic" = lator_endpoint_beta_lactam(
      drug = text("drug"),
      target_fraction = number("target_fraction", 0.4),
      mic_variable = text("mic_variable", "MIC"),
      mic_max_age = number("mic_max_age", 168),
      threshold_multiplier = number("threshold_multiplier", 1),
      free_fraction = number("free_fraction", 1),
      source = source, status = status
    ),
    "template-aminoglycoside" = lator_endpoint_aminoglycoside(
      drug = text("drug"),
      efficacy_lower = number("efficacy_lower"),
      trough_upper = number("trough_upper"), source = source,
      efficacy_upper = number("efficacy_upper", required = FALSE),
      efficacy_metric = text("efficacy_metric", "Cmax/MIC"),
      mic_variable = text("mic_variable", "MIC"),
      mic_max_age = number("mic_max_age", 168),
      unit = text("unit", "ratio"), status = status
    ),
    "template-tacrolimus" = lator_endpoint_tacrolimus(
      lower = number("lower"), upper = number("upper"), unit = text("unit"),
      source = source, drug = text("drug", "tacrolimus"), status = status
    ),
    "template-mycophenolate" = lator_endpoint_mycophenolate(
      lower = number("lower"), upper = number("upper"), unit = text("unit"),
      source = source, drug = text("drug", "mycophenolic acid"),
      status = status
    ),
    "template-busulfan" = lator_endpoint_busulfan(
      lower = number("lower"), upper = number("upper"), unit = text("unit"),
      source = source, doses = number("doses", 1),
      drug = text("drug", "busulfan"), status = status
    ),
    "template-methotrexate" = lator_endpoint_methotrexate(
      targets = .lator_endpoint_template_targets(
        values$targets,
        c("hours_after_dose", "lower", "upper", "tolerance"),
        "Methotrexate timed thresholds"
      ),
      unit = text("unit"), source = source,
      drug = text("drug", "methotrexate"), status = status
    ),
    "template-atg-pre-event" = lator_endpoint_atg(
      drug = text("drug", "ATG"),
      targets = .lator_endpoint_template_targets(
        values$targets,
        c("window_start", "window_end", "lower", "upper"),
        "ATG target windows"
      ),
      unit = text("unit"), source = source,
      anchor = text("anchor", "transplantation"), status = status
    ),
    "template-warfarin" = lator_endpoint_warfarin(
      lower = number("lower", 2), upper = number("upper", 3),
      target_fraction = number("target_fraction", 0.7), source = source,
      drug = text("drug", "warfarin"), status = status
    )
  )
  endpoint$version <- .lator_scalar(version, "version", max_chars = 32L)
  endpoint$metadata$template_id <- template_id
  lator_endpoint_validate(endpoint)
}

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
    constructor = "",
    description = paste(item$drug, item$status, paste0("v", item$version)), stringsAsFactors = FALSE
  )))
  rbind(builtins, registered)
}

#' Register an encrypted endpoint definition
#'
#' Registered endpoint versions are immutable. Re-registering the same
#' `id@version` is rejected; modifications must use a new version.
#' @param workspace Unlocked workspace.
#' @param endpoint Endpoint to register.
#' @param actor Audit actor.
#' @export
lator_endpoint_register <- function(workspace, endpoint, actor = "local-session") {
  workspace <- .lator_require_workspace(workspace)
  endpoint <- lator_endpoint_validate(endpoint)
  .lator_with_lock(workspace, "workspace-write", function() {
    catalog_path <- file.path(workspace$paths$endpoints, "catalog.enc")
    catalog <- .lator_encrypt_read(catalog_path, workspace$key, list(items = list()))
    key <- paste(endpoint$id, endpoint$version, sep = "@")
    if (key %in% names(catalog$items)) {
      .lator_stop(
        "Endpoint ", key,
        " is already registered; create a new endpoint version instead."
      )
    }
    token <- .lator_record_token(key, workspace$key)
    path <- file.path(workspace$paths$endpoints, paste0(token, ".enc"))
    .lator_atomic_encrypt_save(endpoint, path, workspace$key)
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
