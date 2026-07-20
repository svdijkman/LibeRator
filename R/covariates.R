.lator_active_events <- function(patient, types = NULL, name = NULL, cutoff = Inf) {
  patient <- .lator_validate_patient(patient)
  events <- patient$events
  if (!length(events)) return(events)
  superseded <- unique(vapply(events, function(event) {
    as.character(event$supersedes %||% "")
  }, character(1)))
  superseded <- superseded[nzchar(superseded)]
  keep <- vapply(events, function(event) {
    event$time <= cutoff && !event$event_id %in% superseded &&
      (is.null(types) || event$type %in% types) &&
      (is.null(name) || identical(tolower(event$name), tolower(name)))
  }, logical(1))
  events[keep]
}

.lator_event_table <- function(events) {
  if (!length(events)) return(data.frame(
    event_id = character(), time = numeric(), value = numeric(), unit = character(),
    source = character(), missing_reason = character(), stringsAsFactors = FALSE
  ))
  data.frame(
    event_id = vapply(events, `[[`, character(1), "event_id"),
    time = vapply(events, `[[`, numeric(1), "time"),
    value = vapply(events, function(event) suppressWarnings(as.numeric(event$value)), numeric(1)),
    unit = vapply(events, `[[`, character(1), "unit"),
    source = vapply(events, `[[`, character(1), "source"),
    missing_reason = vapply(events, `[[`, character(1), "missing_reason"),
    stringsAsFactors = FALSE
  )
}

.lator_covariate_result <- function(times, name) data.frame(
  name = rep(name, length(times)), time = times, value = rep(NA_real_, length(times)),
  unit = rep("", length(times)), source_time = rep(NA_real_, length(times)),
  age = rep(NA_real_, length(times)), method = rep("none", length(times)),
  status = rep("missing", length(times)), uncertainty_sd = rep(NA_real_, length(times)),
  source_event_id = rep("", length(times)), scheduled_missing = rep(FALSE, length(times)),
  missing_reason = rep("", length(times)), stringsAsFactors = FALSE
)

#' Resolve a longitudinal covariate at specified times
#'
#' Missing observations are never silently converted into population values.
#' Carry-forward, interpolation, nearest-neighbour, and fallback behaviour are
#' explicit policies and every returned value records its provenance and age.
#'
#' @param patient A `lator_patient`.
#' @param name Covariate event name.
#' @param times Numeric timeline hours.
#' @param method One of `"locf"`, `"linear"`, `"nearest"`, or `"none"`.
#' @param max_age Maximum permitted age in hours for carried or nearest values.
#' @param max_gap Maximum interval in hours spanning a linear interpolation.
#' @param fallback Optional explicit fallback value. It is labelled `fallback`,
#'   never as an observation.
#' @param fallback_unit Unit for `fallback` if no observed unit is available.
#' @param uncertainty_sd Optional standard deviation attached to fallback
#'   values. Event-specific `metadata$uncertainty_sd` takes precedence for
#'   observed values.
#' @param cutoff Ignore evidence recorded after this patient-timeline time.
#' @return A provenance-rich data frame with one row per requested time.
#' @export
lator_covariate_at <- function(patient, name, times,
                               method = c("locf", "linear", "nearest", "none"),
                               max_age = Inf, max_gap = Inf, fallback = NULL,
                               fallback_unit = "", uncertainty_sd = NA_real_,
                               cutoff = Inf) {
  patient <- .lator_validate_patient(patient)
  name <- .lator_scalar(name, "name")
  method <- match.arg(method)
  times <- suppressWarnings(as.numeric(times))
  if (!length(times) || any(!is.finite(times))) .lator_stop("`times` must contain finite timeline hours.")
  max_age <- .lator_number(max_age, "max_age", finite = FALSE)
  max_gap <- .lator_number(max_gap, "max_gap", finite = FALSE)
  if (max_age < 0 || max_gap < 0) .lator_stop("Covariate age and gap limits cannot be negative.")

  events <- .lator_active_events(patient, types = "covariate", name = name, cutoff = cutoff)
  table <- .lator_event_table(events)
  usable <- is.finite(table$value)
  observed <- table[usable, , drop = FALSE]
  observed_events <- events[usable]
  if (nrow(observed)) {
    ordering <- order(observed$time, observed$event_id)
    observed <- observed[ordering, , drop = FALSE]
    observed_events <- observed_events[ordering]
    observed$uncertainty_sd <- vapply(observed_events, function(event) {
      value <- suppressWarnings(as.numeric(event$metadata$uncertainty_sd %||% NA_real_))
      if (length(value) != 1L || !is.finite(value) || value < 0) NA_real_ else value
    }, numeric(1))
  }
  result <- .lator_covariate_result(times, name)
  if (nrow(table)) for (row in seq_along(times)) {
    missed <- which(table$time == times[row] & !is.finite(table$value) & nzchar(table$missing_reason))
    if (length(missed)) {
      result$scheduled_missing[row] <- TRUE
      result$missing_reason[row] <- paste(unique(table$missing_reason[missed]), collapse = "; ")
    }
  }

  assign_observation <- function(row, index, used_method, signed_age = NULL) {
    result$value[row] <<- observed$value[index]
    result$unit[row] <<- observed$unit[index]
    result$source_time[row] <<- observed$time[index]
    result$age[row] <<- signed_age %||% (times[row] - observed$time[index])
    result$method[row] <<- used_method
    result$status[row] <<- if (identical(times[row], observed$time[index])) "observed" else
      if (isTRUE(result$scheduled_missing[row])) "resolved_after_missing" else "resolved"
    result$uncertainty_sd[row] <<- observed$uncertainty_sd[index]
    result$source_event_id[row] <<- observed$event_id[index]
  }

  if (nrow(observed)) for (row in seq_along(times)) {
    exact <- which(observed$time == times[row])
    if (length(exact)) {
      assign_observation(row, utils::tail(exact, 1L), "observed", 0)
      next
    }
    if (method == "locf") {
      available <- which(observed$time < times[row])
      if (length(available)) {
        index <- utils::tail(available, 1L)
        if (times[row] - observed$time[index] <= max_age) assign_observation(row, index, "locf")
        else result$status[row] <- "stale"
      }
    } else if (method == "nearest") {
      distance <- abs(observed$time - times[row])
      index <- which.min(distance)
      if (distance[index] <= max_age) assign_observation(row, index, "nearest", distance[index])
      else result$status[row] <- "stale"
    } else if (method == "linear") {
      before <- which(observed$time < times[row])
      after <- which(observed$time > times[row])
      if (length(before) && length(after)) {
        left <- utils::tail(before, 1L); right <- after[1L]
        gap <- observed$time[right] - observed$time[left]
        if (gap <= max_gap &&
            times[row] - observed$time[left] <= max_age &&
            observed$time[right] - times[row] <= max_age &&
            identical(observed$unit[left], observed$unit[right])) {
          weight <- (times[row] - observed$time[left]) / gap
          result$value[row] <- observed$value[left] * (1 - weight) + observed$value[right] * weight
          result$unit[row] <- observed$unit[left]
          result$source_time[row] <- observed$time[left]
          result$age[row] <- max(times[row] - observed$time[left], observed$time[right] - times[row])
          result$method[row] <- "linear"
          result$status[row] <- "resolved"
          result$source_event_id[row] <- paste(observed$event_id[c(left, right)], collapse = "+")
          if (all(is.finite(observed$uncertainty_sd[c(left, right)]))) {
            result$uncertainty_sd[row] <- sqrt(
              ((1 - weight) * observed$uncertainty_sd[left])^2 +
                (weight * observed$uncertainty_sd[right])^2
            )
          }
        } else result$status[row] <- "stale"
      }
    }
  }

  unresolved <- !is.finite(result$value)
  if (any(unresolved) && !is.null(fallback)) {
    fallback <- .lator_number(fallback, "fallback")
    result$value[unresolved] <- fallback
    result$unit[unresolved] <- as.character(fallback_unit %||% "")
    result$method[unresolved] <- "fallback"
    result$status[unresolved] <- "fallback"
    result$uncertainty_sd[unresolved] <- suppressWarnings(as.numeric(uncertainty_sd))[1L]
  }
  result
}

.lator_resolve_covariates <- function(patient, names, times, policies = list(), cutoff = Inf) {
  if (!length(names)) return(list(data = data.frame(time = times), evidence = list(), warnings = character()))
  data <- data.frame(time = times)
  evidence <- list(); warnings <- character()
  for (name in unique(names)) {
    policy <- policies[[name]] %||% policies[[toupper(name)]] %||% list(method = "locf")
    if (!is.list(policy)) .lator_stop("Covariate policies must be named argument lists.")
    resolved <- do.call(lator_covariate_at, c(
      list(patient = patient, name = name, times = times, cutoff = cutoff), policy
    ))
    data[[name]] <- resolved$value
    evidence[[name]] <- resolved
    if (any(resolved$status == "fallback")) warnings <- c(
      warnings, sprintf("%s uses an explicit fallback for %d row(s).", name, sum(resolved$status == "fallback"))
    )
    if (any(resolved$scheduled_missing)) warnings <- c(
      warnings, sprintf(
        "%s had %d explicitly missing scheduled measurement(s); the configured policy is retained in provenance.",
        name, length(unique(paste(
          resolved$time[resolved$scheduled_missing],
          resolved$missing_reason[resolved$scheduled_missing], sep = "|"
        )))
      )
    )
    if (any(!is.finite(resolved$value))) warnings <- c(
      warnings, sprintf("%s remains unresolved for %d row(s).", name, sum(!is.finite(resolved$value)))
    )
  }
  list(data = data, evidence = evidence, warnings = unique(warnings))
}
