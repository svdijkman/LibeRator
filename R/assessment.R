.lator_match_therapy_events <- function(patient, types, analyte) {
  events <- .lator_active_events(patient, types = types)
  if (!length(events)) return(events)
  matched <- Filter(function(event) {
    event_target <- if (identical(event$type, "concentration")) {
      event$metadata$analyte %||% event$name
    } else {
      event$metadata$drug %||% event$name
    }
    event_target <- as.character(event_target %||% "")
    identical(tolower(trimws(event_target)), tolower(trimws(analyte)))
  }, events)
  matched
}

.lator_assessment_input_hashes <- function(patient, analyte) {
  patient <- .lator_validate_patient(patient)
  therapy <- lator_patient_endpoint_get(patient, analyte) %||% list()
  therapy_context <- therapy[intersect(
    c(
      "drug", "therapeutic_class", "monitoring_analytes",
      "treatment_status", "added_at"
    ),
    names(therapy)
  )]
  doses <- .lator_match_therapy_events(patient, "dose", analyte)
  tdm <- .lator_match_therapy_events(patient, "concentration", analyte)
  context <- .lator_active_events(patient)
  context <- Filter(function(event) {
    !event$type %in% c("dose", "concentration")
  }, context)
  list(
    medication_hash = .lator_hash(list(
      therapy = therapy_context, doses = doses
    )),
    tdm_hash = .lator_hash(tdm),
    patient_context_hash = .lator_hash(context)
  )
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

.lator_dynamic_evidence_status <- function(patient, analyte, cutoff = Inf,
                                           state_times = NULL) {
  observations <- .lator_match_therapy_events(
    patient, "concentration", analyte
  )
  observations <- Filter(function(event) {
    event$time <= cutoff &&
      is.finite(suppressWarnings(as.numeric(event$value)))
  }, observations)
  doses <- .lator_match_therapy_events(patient, "dose", analyte)
  doses <- Filter(function(event) event$time <= cutoff, doses)
  if (!length(observations)) {
    return(list(
      ready = FALSE, state_count = 0L, observed_state_count = 0L,
      boundary_count = 0L,
      reason = "Time-changing estimation requires measured TDM evidence."
    ))
  }
  model_times <- c(
    vapply(doses, `[[`, numeric(1), "time"),
    vapply(observations, `[[`, numeric(1), "time")
  )
  first_model_time <- min(model_times)
  latest_observation <- max(vapply(
    observations, `[[`, numeric(1), "time"
  ))
  boundaries <- .lator_boundary_times(patient, cutoff, state_times)
  boundaries <- boundaries[
    boundaries > first_model_time & boundaries <= latest_observation
  ]
  observation_times <- vapply(
    observations, `[[`, numeric(1), "time"
  )
  observed_states <- unique(1L + findInterval(observation_times, boundaries))
  ready <- length(observed_states) >= 2L
  reason <- if (!length(boundaries)) {
    paste(
      "Time-changing estimation needs at least two patient states.",
      "Add a State boundary between clinically distinct periods, with TDM",
      "evidence on both sides, or estimate stable patient parameters."
    )
  } else if (!ready) {
    paste(
      "The declared state boundaries currently leave measured TDM evidence",
      "in only one state. Add a TDM measurement in another state or estimate",
      "stable patient parameters."
    )
  } else {
    paste(
      length(observed_states), "patient states contain measured TDM evidence."
    )
  }
  list(
    ready = ready,
    state_count = as.integer(length(boundaries) + 1L),
    observed_state_count = as.integer(length(observed_states)),
    boundary_count = as.integer(length(boundaries)),
    reason = reason
  )
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

  treatment_covariates <- .lator_treatment_covariates(
    patient, model$COVARIATES, data$TIME
  )
  measured_covariates <- setdiff(
    model$COVARIATES, names(treatment_covariates)
  )
  covariates <- .lator_resolve_covariates(
    patient, measured_covariates, data$TIME, covariate_policies, cutoff
  )
  for (name in measured_covariates) data[[name]] <- covariates$data[[name]]
  for (name in names(treatment_covariates)) {
    data[[name]] <- treatment_covariates[[name]]$value
    covariates$evidence[[name]] <- treatment_covariates[[name]]$evidence
  }
  # A pre-change row must use the previous effective covariate value. This is
  # distinct from LOCF at the exact time, which correctly selects the new value.
  prechange <- data$.LATOR_ROLE == "prechange"
  if (any(prechange) && length(measured_covariates)) for (name in measured_covariates) {
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

.lator_model_with_individual_outputs <- function(model) {
  catalog <- LibeRation::nm_model_outputs(model)
  assigned <- catalog$name[
    catalog$source %in% c("model assignment", "post-ADVAN prediction") &
      catalog$selectable %in% TRUE
  ]
  model$OUTPUT <- unique(c(model$OUTPUT %||% character(), assigned))
  model
}

.lator_individual_parameters <- function(fit) {
  predictions <- as.data.frame(fit$predictions)
  selected <- intersect(
    fit$model$OUTPUT %||% character(), names(predictions)
  )
  selected <- setdiff(
    selected,
    c("PRED", "IPRED", "RES", "IRES", "WRES", "IWRES", "CWRES",
      grep("^ETA[0-9]", selected, value = TRUE),
      grep("^(A|S)[0-9]+$", selected, value = TRUE))
  )
  details <- .lator_model_output_details(fit$model, selected)
  selected <- details$name[details$kind != "internal"]
  details <- details[match(selected, details$name), , drop = FALSE]
  if (!length(selected)) {
    return(data.frame(
      occasion = integer(), time = numeric(), parameter = character(),
      kind = character(), expression = character(), individualised = logical(),
      value = numeric(), stringsAsFactors = FALSE
    ))
  }
  occasion <- if ("OCC" %in% names(predictions)) {
    as.integer(predictions$OCC)
  } else rep(1L, nrow(predictions))
  groups <- split(seq_len(nrow(predictions)), occasion)
  rows <- unlist(lapply(names(groups), function(group) {
    indices <- groups[[group]]
    lapply(selected, function(parameter) {
      values <- suppressWarnings(as.numeric(predictions[[parameter]][indices]))
      usable <- which(is.finite(values))
      if (!length(usable)) return(NULL)
      index <- indices[[utils::tail(usable, 1L)]]
      data.frame(
        occasion = as.integer(group),
        time = as.numeric(predictions$TIME[[index]]),
        parameter = parameter,
        kind = details$kind[match(parameter, details$name)],
        expression = details$expression[match(parameter, details$name)],
        individualised = details$individualised[
          match(parameter, details$name)
        ],
        value = as.numeric(predictions[[parameter]][[index]]),
        stringsAsFactors = FALSE
      )
    })
  }), recursive = FALSE)
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(
      occasion = integer(), time = numeric(), parameter = character(),
      kind = character(), expression = character(), individualised = logical(),
      value = numeric(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

.lator_postdose_window <- function(patient, analyte, cutoff = Inf,
                                   fallback_interval = 24) {
  doses <- .lator_match_therapy_events(patient, "dose", analyte)
  doses <- Filter(function(event) event$time <= cutoff, doses)
  if (!length(doses)) return(NULL)
  times <- sort(unique(vapply(doses, `[[`, numeric(1), "time")))
  latest_time <- utils::tail(times, 1L)
  latest <- utils::tail(Filter(function(event) {
    identical(as.numeric(event$time), latest_time)
  }, doses), 1L)[[1L]]
  explicit <- suppressWarnings(as.numeric(latest$metadata$ii %||% NA_real_))
  if (length(explicit) == 1L && is.finite(explicit) && explicit > 0) {
    return(list(
      start = latest_time, interval = explicit,
      source = "recorded dosing interval", assumed = FALSE
    ))
  }
  gaps <- diff(times)
  gaps <- gaps[is.finite(gaps) & gaps > 0]
  if (length(gaps)) {
    recent <- utils::tail(gaps, 4L)
    return(list(
      start = latest_time, interval = stats::median(recent),
      source = "recent dose-time spacing", assumed = FALSE
    ))
  }
  list(
    start = latest_time,
    interval = .lator_number(
      fallback_interval, "fallback_interval", positive = TRUE
    ),
    source = "24-hour fallback; no dosing interval was recorded",
    assumed = TRUE
  )
}

.lator_dose_signature <- function(event) {
  metadata <- event$metadata %||% list()
  paste(
    formatC(as.numeric(event$value), digits = 12, format = "fg"),
    formatC(as.numeric(metadata$ii %||% 0), digits = 12, format = "fg"),
    formatC(as.numeric(metadata$rate %||% 0), digits = 12, format = "fg"),
    tolower(trimws(as.character(metadata$route %||% ""))),
    as.integer(metadata$cmt %||% 1L),
    sep = "|"
  )
}

.lator_profile_observation_selection <- function(
    patient, analyte, cutoff = Inf,
    scope = c("automatic", "latest", "last_n", "all", "since"),
    count = 2L, since = NA_real_, episode_gap_hours = 1008) {
  scope <- match.arg(scope)
  episode_gap_hours <- .lator_number(
    episode_gap_hours, "profile_episode_gap_hours", positive = TRUE
  )
  observations <- .lator_match_therapy_events(
    patient, "concentration", analyte
  )
  observations <- Filter(function(event) {
    event$time <= cutoff &&
      is.finite(suppressWarnings(as.numeric(event$value)))
  }, observations)
  if (!length(observations)) {
    return(list(
      events = list(), scope = scope, label = "No measured TDM",
      total_count = 0L, selected_count = 0L,
      episode_start = NA_real_, episode_gap_hours = episode_gap_hours
    ))
  }
  ordering <- order(vapply(observations, `[[`, numeric(1), "time"))
  observations <- observations[ordering]
  times <- vapply(observations, `[[`, numeric(1), "time")
  selected <- observations
  episode_start <- -Inf
  label <- "All historical TDM measurements"
  if (identical(scope, "latest")) {
    selected <- utils::tail(observations, 1L)
    episode_start <- utils::tail(times, 1L)
    label <- "Most recent TDM measurement"
  } else if (identical(scope, "last_n")) {
    count <- as.integer(count)
    if (length(count) != 1L || is.na(count) || count < 1L) {
      .lator_stop("`profile_observation_count` must be a positive integer.")
    }
    selected <- utils::tail(observations, count)
    episode_start <- vapply(selected, `[[`, numeric(1), "time")[[1L]]
    label <- paste("Most recent", length(selected), "TDM measurement(s)")
  } else if (identical(scope, "since")) {
    since <- .lator_number(since, "profile_observation_since")
    selected <- Filter(function(event) event$time >= since, observations)
    episode_start <- since
    label <- paste("TDM measurements since timeline hour", format(since))
  } else if (identical(scope, "automatic")) {
    starts <- times[[1L]]
    gaps <- diff(times)
    if (length(gaps) && any(gaps > episode_gap_hours)) {
      starts <- c(starts, times[which(gaps > episode_gap_hours) + 1L])
    }
    latest_tdm <- utils::tail(times, 1L)
    boundaries <- .lator_boundary_times(patient, cutoff)
    boundaries <- boundaries[boundaries <= latest_tdm]
    if (length(boundaries)) starts <- c(starts, utils::tail(boundaries, 1L))
    doses <- .lator_match_therapy_events(patient, "dose", analyte)
    doses <- Filter(function(event) event$time <= latest_tdm, doses)
    if (length(doses) > 1L) {
      doses <- doses[order(vapply(doses, `[[`, numeric(1), "time"))]
      signatures <- vapply(doses, .lator_dose_signature, character(1))
      changed <- which(signatures[-1L] != signatures[-length(signatures)]) + 1L
      if (length(changed)) {
        starts <- c(
          starts,
          vapply(doses[changed], `[[`, numeric(1), "time")
        )
      }
    }
    episode_start <- max(starts[starts <= latest_tdm])
    selected <- Filter(function(event) {
      event$time >= episode_start
    }, observations)
    label <- paste(
      "Current monitoring episode:",
      length(selected), "of", length(observations), "TDM measurement(s)"
    )
  }
  if (!length(selected)) {
    .lator_stop(
      "The selected profile display window contains no measured TDM points."
    )
  }
  list(
    events = selected, scope = scope, label = label,
    total_count = as.integer(length(observations)),
    selected_count = as.integer(length(selected)),
    episode_start = if (is.finite(episode_start)) episode_start else NA_real_,
    episode_gap_hours = episode_gap_hours
  )
}

.lator_profile_observation_rows <- function(
    patient, analyte, cutoff, window,
    scope = "automatic", count = 2L, since = NA_real_,
    episode_gap_hours = 1008) {
  selection <- .lator_profile_observation_selection(
    patient, analyte, cutoff, scope, count, since, episode_gap_hours
  )
  doses <- .lator_match_therapy_events(patient, "dose", analyte)
  doses <- Filter(function(event) event$time <= cutoff, doses)
  doses <- doses[order(vapply(doses, `[[`, numeric(1), "time"))]
  rows <- lapply(selection$events, function(event) {
    preceding <- Filter(function(dose) dose$time <= event$time, doses)
    anchor <- if (length(preceding)) {
      utils::tail(preceding, 1L)[[1L]]
    } else NULL
    anchor_time <- if (is.null(anchor)) window$start else anchor$time
    recorded_interval <- if (is.null(anchor)) NA_real_ else {
      suppressWarnings(as.numeric(anchor$metadata$ii %||% NA_real_))
    }
    interval <- if (
      length(recorded_interval) == 1L && is.finite(recorded_interval) &&
        recorded_interval > 0
    ) recorded_interval else window$interval
    elapsed <- event$time - anchor_time
    phase <- elapsed
    tolerance <- sqrt(.Machine$double.eps) * max(1, interval)
    if (is.finite(elapsed) && elapsed > interval + tolerance) {
      phase <- elapsed %% interval
      if (phase < tolerance || interval - phase < tolerance) {
        phase <- interval
      }
    }
    phase <- min(max(phase, 0), window$interval)
    data.frame(
      TIME = window$start + phase,
      IPRED = NA_real_,
      DV = as.numeric(event$value),
      kind = "observation",
      OBSERVED_TIME = as.numeric(event$time),
      EVENT_ID = as.character(event$event_id),
      stringsAsFactors = FALSE
    )
  })
  list(
    data = if (length(rows)) do.call(rbind, rows) else data.frame(),
    selection = selection
  )
}

.lator_dose_context_warnings <- function(patient, analyte, cutoff = Inf) {
  doses <- .lator_match_therapy_events(patient, "dose", analyte)
  doses <- Filter(function(event) event$time <= cutoff, doses)
  if (!length(doses)) return(character())
  interval_without_steady_state <- vapply(doses, function(event) {
    interval <- suppressWarnings(as.numeric(event$metadata$ii %||% 0))
    steady_state <- suppressWarnings(as.integer(event$metadata$ss %||% 0L))
    length(interval) == 1L && is.finite(interval) && interval > 0 &&
      (!length(steady_state) || is.na(steady_state) || steady_state == 0L)
  }, logical(1))
  if (!any(interval_without_steady_state)) return(character())
  paste(
    "A dosing interval was recorded without steady-state dosing.",
    "Only the administrations present on the timeline contribute to the fit;",
    "select 'Steady state before this dose' when the concentration follows an",
    "established regular regimen."
  )
}

.lator_profile_statistics <- function(curve) {
  curve <- as.data.frame(curve)
  if (!nrow(curve) || !all(c("TIME", "IPRED") %in% names(curve))) {
    return(NULL)
  }
  time <- suppressWarnings(as.numeric(curve$TIME))
  value <- suppressWarnings(as.numeric(curve$IPRED))
  keep <- is.finite(time) & is.finite(value)
  if (!any(keep)) return(NULL)
  time <- time[keep]
  value <- value[keep]
  ordering <- order(time)
  time <- time[ordering]
  value <- value[ordering]
  duration <- diff(range(time))
  mean_css <- if (is.finite(duration) && duration > 0) {
    .lator_trapz(time, value) / duration
  } else value[[length(value)]]
  trough <- min(value)
  peak <- max(value)
  list(
    mean_css = mean_css,
    trough = trough,
    peak = peak,
    fluctuation_percent = if (is.finite(mean_css) &&
      abs(mean_css) > sqrt(.Machine$double.eps)) {
      100 * (peak - trough) / abs(mean_css)
    } else NA_real_
  )
}

.lator_first_primes <- function(n) {
  n <- as.integer(n)
  if (n < 1L) return(integer())
  primes <- integer()
  candidate <- 2L
  while (length(primes) < n) {
    limit <- floor(sqrt(candidate))
    divisors <- if (limit >= 2L) 2L:limit else integer()
    if (!length(divisors) || all(candidate %% divisors != 0L)) {
      primes <- c(primes, candidate)
    }
    candidate <- candidate + 1L
  }
  primes
}

.lator_halton_column <- function(n, base) {
  output <- numeric(n)
  for (index in seq_len(n)) {
    value <- index
    fraction <- 1 / base
    while (value > 0L) {
      output[[index]] <- output[[index]] +
        fraction * (value %% base)
      value <- value %/% base
      fraction <- fraction / base
    }
  }
  output
}

.lator_population_eta_draws <- function(model, n, eta_columns = model$n_eta) {
  n <- as.integer(n)
  eta_columns <- as.integer(eta_columns)
  covariance <- .lator_omega_matrix(model)
  base_eta <- nrow(covariance)
  if (n < 1L || eta_columns < 1L || base_eta < 1L) {
    return(matrix(numeric(), max(n, 0L), max(eta_columns, 0L)))
  }
  bases <- .lator_first_primes(base_eta)
  uniforms <- vapply(
    bases, function(base) .lator_halton_column(n, base), numeric(n)
  )
  uniforms <- pmin(pmax(uniforms, 1e-10), 1 - 1e-10)
  standard <- stats::qnorm(uniforms)
  eig <- eigen((covariance + t(covariance)) / 2, symmetric = TRUE)
  root <- eig$vectors %*% diag(
    sqrt(pmax(eig$values, 0)), base_eta
  )
  base_draws <- standard %*% t(root)
  occasions <- ceiling(eta_columns / base_eta)
  draws <- do.call(cbind, rep(list(base_draws), occasions))
  draws[, seq_len(eta_columns), drop = FALSE]
}

.lator_population_profile_interval <- function(
    fit, data, keep, draws = 200L,
    probs = c(0.025, 0.5, 0.975)) {
  draws <- as.integer(draws)
  probs <- as.numeric(probs)
  if (is.na(draws) || draws < 1L) return(data.frame())
  if (length(probs) != 3L || any(!is.finite(probs)) ||
      any(probs <= 0 | probs >= 1) || any(diff(probs) <= 0)) {
    .lator_stop(
      "`profile_interval_probs` must contain three increasing probabilities ",
      "between zero and one."
    )
  }
  eta <- .lator_population_eta_draws(
    fit$model, draws, eta_columns = length(fit$eta)
  )
  replicated <- .lator_replicate_dataset(data, draws, "PROFILE")
  simulated <- LibeRation::nm_simulate(
    fit$model, replicated, theta = fit$theta, eta = eta,
    sigma = fit$sigma, omega = fit$omega, residual = FALSE
  )
  selected <- simulated[keep(simulated), , drop = FALSE]
  selected <- selected[
    is.finite(suppressWarnings(as.numeric(selected$TIME))) &
      is.finite(suppressWarnings(as.numeric(selected$IPRED))), ,
    drop = FALSE
  ]
  if (!nrow(selected)) return(data.frame())
  groups <- split(
    as.numeric(selected$IPRED), as.numeric(selected$TIME)
  )
  rows <- lapply(names(groups), function(time) {
    values <- groups[[time]]
    quantiles <- stats::quantile(
      values, probs = probs, names = FALSE, type = 8, na.rm = TRUE
    )
    data.frame(
      TIME = as.numeric(time),
      POP_LOWER = quantiles[[1L]],
      POP_MEDIAN = quantiles[[2L]],
      POP_UPPER = quantiles[[3L]],
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  result[order(result$TIME), , drop = FALSE]
}

.lator_dense_individual_profile <- function(
    patient, fit, analyte, cutoff, covariate_policies,
    dynamic = FALSE, state_times = NULL, grid_step = 0.25,
    population_draws = 200L,
    interval_probs = c(0.025, 0.5, 0.975),
    observation_scope = "automatic", observation_count = 2L,
    observation_since = NA_real_, episode_gap_hours = 1008) {
  window <- .lator_postdose_window(patient, analyte, cutoff)
  if (is.null(window)) {
    return(list(
      profile = data.frame(), interval = NULL,
      warning = "No matching dose was available for a post-dose PK profile."
    ))
  }
  grid_step <- .lator_number(grid_step, "grid_step", positive = TRUE)
  end <- window$start + window$interval
  grid <- seq(window$start, end, by = grid_step)
  if (!length(grid) || utils::tail(grid, 1L) < end) {
    grid <- c(grid, end)
  }
  grid <- sort(unique(c(window$start, grid, end)))
  observation_result <- .lator_profile_observation_rows(
    patient, analyte, cutoff, window,
    scope = observation_scope, count = observation_count,
    since = observation_since, episode_gap_hours = episode_gap_hours
  )
  observed <- observation_result$data
  direct_mean_only <- identical(
    .lator_model_profile_type(fit$model), "steady_state_mean_only"
  )
  if (direct_mean_only) {
    observed_predictions <- as.data.frame(fit$predictions)
    usable <- which(
      observed_predictions$EVID == 0L &
        is.finite(suppressWarnings(as.numeric(observed_predictions$IPRED)))
    )
    if (!length(usable)) {
      return(list(
        profile = data.frame(), interval = NULL,
        warning = paste(
          "The selected direct $PRED model did not expose a finite",
          "steady-state concentration."
        )
      ))
    }
    estimate <- as.numeric(
      observed_predictions$IPRED[[utils::tail(usable, 1L)]]
    )
    curve <- data.frame(
      TIME = c(window$start, end), IPRED = estimate,
      DV = NA_real_, kind = "curve", OBSERVED_TIME = NA_real_,
      EVENT_ID = "", stringsAsFactors = FALSE
    )
    population_result <- tryCatch(
      list(
        data = .lator_population_profile_interval(
          fit, fit$data,
          keep = function(data) data$EVID == 0L,
          draws = population_draws, probs = interval_probs
        ),
        warning = ""
      ),
      error = function(error) list(
        data = data.frame(), warning = conditionMessage(error)
      )
    )
    population_interval <- population_result$data
    if (nrow(population_interval)) {
      latest <- population_interval[
        which.max(population_interval$TIME), , drop = FALSE
      ]
      curve$POP_LOWER <- latest$POP_LOWER
      curve$POP_MEDIAN <- latest$POP_MEDIAN
      curve$POP_UPPER <- latest$POP_UPPER
    }
    for (name in setdiff(names(curve), names(observed))) {
      observed[[name]] <- NA_real_
    }
    observed <- observed[, names(curve), drop = FALSE]
    profile <- rbind(curve, observed)
    profile <- profile[order(profile$TIME, profile$kind), , drop = FALSE]
    return(list(
      profile = profile,
      interval = list(
        start = window$start, end = end, hours = window$interval,
        source = window$source, assumed = isTRUE(window$assumed),
        grid_step_hours = grid_step,
        profile_type = "steady_state_mean_only",
        summary = .lator_profile_statistics(curve),
        observation_selection = observation_result$selection[
          setdiff(names(observation_result$selection), "events")
        ],
        population_interval = if (nrow(population_interval)) list(
          lower = latest$POP_LOWER[[1L]],
          median = latest$POP_MEDIAN[[1L]],
          upper = latest$POP_UPPER[[1L]],
          probabilities = interval_probs,
          draws = as.integer(population_draws),
          variability = "between-subject ETA variability only"
        ) else NULL
      ),
      warning = paste(c(
        "This direct $PRED model estimates an average steady-state",
        "concentration only; it cannot identify a within-interval PK curve,",
        "peak, or trough.",
        if (nzchar(population_result$warning)) paste(
          "Population prediction interval could not be generated:",
          population_result$warning
        )
      ), collapse = " ")
    ))
  }
  future <- data.frame(
    TIME = grid, EVID = 0L, AMT = 0, RATE = 0,
    II = 0, SS = 0L, ADDL = 0L, CMT = fit$model$OBSCMP,
    stringsAsFactors = FALSE
  )
  prepared <- .lator_patient_dataset(
    patient, fit$model, analyte, cutoff = cutoff,
    covariate_policies = covariate_policies,
    dynamic = isTRUE(dynamic), state_times = state_times,
    include_future = future
  )
  simulated <- LibeRation::nm_simulate(
    fit$model, prepared$data, theta = fit$theta,
    eta = matrix(fit$eta, nrow = 1L), sigma = fit$sigma,
    omega = fit$omega
  )
  future_rows <- simulated$.LATOR_ROLE %in% "future" &
    simulated$TIME >= window$start & simulated$TIME <= end
  curve <- data.frame(
    TIME = as.numeric(simulated$TIME[future_rows]),
    IPRED = as.numeric(simulated$IPRED[future_rows]),
    DV = NA_real_, kind = "curve", OBSERVED_TIME = NA_real_,
    EVENT_ID = "", stringsAsFactors = FALSE
  )
  population_result <- tryCatch(
    list(
      data = .lator_population_profile_interval(
        fit, prepared$data,
        keep = function(data) {
          data$.LATOR_ROLE %in% "future" &
            data$TIME >= window$start & data$TIME <= end
        },
        draws = population_draws, probs = interval_probs
      ),
      warning = ""
    ),
    error = function(error) list(
      data = data.frame(), warning = conditionMessage(error)
    )
  )
  population_interval <- population_result$data
  if (nrow(population_interval)) {
    matched <- match(
      round(curve$TIME, 10), round(population_interval$TIME, 10)
    )
    curve$POP_LOWER <- population_interval$POP_LOWER[matched]
    curve$POP_MEDIAN <- population_interval$POP_MEDIAN[matched]
    curve$POP_UPPER <- population_interval$POP_UPPER[matched]
  }
  for (name in setdiff(names(curve), names(observed))) {
    observed[[name]] <- NA_real_
  }
  observed <- observed[, names(curve), drop = FALSE]
  profile <- rbind(curve, observed)
  profile <- profile[order(profile$TIME, profile$kind), , drop = FALSE]
  rownames(profile) <- NULL
  list(
    profile = profile,
    interval = list(
      start = window$start, end = end, hours = window$interval,
      source = window$source, assumed = isTRUE(window$assumed),
      grid_step_hours = grid_step,
      profile_type = "time_course",
      summary = .lator_profile_statistics(curve),
      observation_selection = observation_result$selection[
        setdiff(names(observation_result$selection), "events")
      ],
      population_interval = if (nrow(population_interval)) list(
        probabilities = interval_probs,
        draws = as.integer(population_draws),
        variability = "between-subject ETA variability only"
      ) else NULL
    ),
    warning = paste(c(
      if (isTRUE(window$assumed)) paste0(
        "The post-dose profile uses a 24-hour fallback because no dosing ",
        "interval or repeated dose spacing was available."
      ),
      if (nzchar(population_result$warning)) paste(
        "Population prediction interval could not be generated:",
        population_result$warning
      )
    ), collapse = " ")
  )
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
#' @param endpoint Versioned therapeutic endpoint or multi-endpoint objective
#'   created with [lator_endpoint_set()].
#' @param analyte Event drug/analyte name; defaults to `endpoint$drug`.
#' @param cutoff Patient-timeline hour through which evidence is visible.
#' @param mode `"static"` or `"dynamic"` latent patient parameters.
#' @param state_times Optional dynamic-state boundaries; otherwise explicit
#'   `state_boundary` events are used.
#' @param covariate_policies Named per-covariate policy lists passed to
#'   [lator_covariate_at()].
#' @param process_scale Random-walk innovation covariance as a multiple of OMEGA.
#' @param process_covariance Optional explicit innovation covariance.
#' @param profile_population_draws Number of deterministic low-discrepancy
#'   OMEGA draws used for the similar-patient population prediction interval.
#'   Set to zero to disable the interval.
#' @param profile_interval_probs Three probabilities for the lower, median and
#'   upper similar-patient interval. Residual error is deliberately excluded.
#' @param profile_observation_scope TDM observations shown over the
#'   individualised dosing-interval profile: `"automatic"` identifies the
#'   current monitoring episode, `"latest"` shows one point, `"last_n"` shows
#'   the requested number, `"all"` shows all history, and `"since"` uses an
#'   explicit patient-timeline hour. This changes display selection only; all
#'   evidence through `cutoff` remains available to the fit.
#' @param profile_observation_count Number of observations for `"last_n"`.
#' @param profile_observation_since Patient-timeline hour for `"since"`.
#' @param profile_episode_gap_hours Gap used to separate monitoring episodes
#'   under the automatic policy. The default is six weeks.
#' @param workspace Optional workspace. When supplied, the immutable assessment
#'   is appended to and saves the patient record.
#' @param actor Audit actor used when saving.
#' @param ... Additional arguments to [LibeRation::nm_individual_fit()].
#' @export
lator_assess <- function(patient, model, endpoint, analyte = endpoint$drug,
                         cutoff = Inf, mode = c("static", "dynamic"),
                         state_times = NULL, covariate_policies = list(),
                         process_scale = 0.1, process_covariance = NULL,
                         profile_population_draws = 200L,
                         profile_interval_probs = c(0.025, 0.5, 0.975),
                         profile_observation_scope = c(
                           "automatic", "latest", "last_n", "all", "since"
                         ),
                         profile_observation_count = 2L,
                         profile_observation_since = NA_real_,
                         profile_episode_gap_hours = 1008,
                         workspace = NULL, actor = "local-session", ...) {
  started <- proc.time()[["elapsed"]]
  patient <- .lator_validate_patient(patient)
  endpoint <- lator_endpoint_validate(endpoint)
  if (!inherits(model, "nm_model")) .lator_stop("`model` must be a LibeRation nm_model.")
  .lator_reject_known_model_defects(model)
  mode <- match.arg(mode)
  profile_observation_scope <- match.arg(profile_observation_scope)
  cutoff <- .lator_number(cutoff, "cutoff", finite = FALSE)
  profile_population_draws <- as.integer(profile_population_draws)
  if (length(profile_population_draws) != 1L ||
      is.na(profile_population_draws) || profile_population_draws < 0L) {
    .lator_stop("`profile_population_draws` must be a non-negative integer.")
  }
  profile_interval_probs <- as.numeric(profile_interval_probs)
  if (length(profile_interval_probs) != 3L ||
      any(!is.finite(profile_interval_probs)) ||
      any(profile_interval_probs <= 0 | profile_interval_probs >= 1) ||
      any(diff(profile_interval_probs) <= 0)) {
    .lator_stop(
      "`profile_interval_probs` must contain three increasing probabilities ",
      "between zero and one."
    )
  }
  profile_episode_gap_hours <- .lator_number(
    profile_episode_gap_hours, "profile_episode_gap_hours", positive = TRUE
  )
  # Validate the display window before starting the relatively expensive fit.
  .lator_profile_observation_selection(
    patient, analyte, cutoff,
    scope = profile_observation_scope,
    count = profile_observation_count,
    since = profile_observation_since,
    episode_gap_hours = profile_episode_gap_hours
  )
  prepared_model <- if (mode == "dynamic") .lator_dynamic_model(model) else model
  prepared_model <- .lator_model_with_individual_outputs(prepared_model)
  prepared <- .lator_patient_dataset(
    patient, prepared_model, analyte, cutoff, covariate_policies,
    dynamic = mode == "dynamic", state_times = state_times
  )
  fit_arguments <- list(model = prepared_model, data = prepared$data)
  if (mode == "dynamic") {
    occasions <- max(prepared$data$OCC)
    observed_states <- unique(prepared$data$OCC[
      prepared$data$EVID == 0L & prepared$data$MDV == 0L
    ])
    if (occasions < 2L || length(observed_states) < 2L) {
      status <- .lator_dynamic_evidence_status(
        patient, analyte, cutoff, state_times
      )
      .lator_stop(status$reason)
    }
    fit_arguments$prior_mean <- rep(0, model$n_eta * occasions)
    fit_arguments$prior_covariance <- .lator_random_walk_covariance(
      model, occasions, process_scale, process_covariance
    )
  }
  fit <- do.call(LibeRation::nm_individual_fit, c(fit_arguments, list(...)))
  trajectory <- .lator_eta_trajectory(fit, model$n_eta, prepared$boundaries)
  dense_profile <- tryCatch(
    .lator_dense_individual_profile(
      patient, fit, analyte, cutoff, covariate_policies,
      dynamic = mode == "dynamic",
      state_times = prepared$boundaries, grid_step = 0.25,
      population_draws = profile_population_draws,
      interval_probs = profile_interval_probs,
      observation_scope = profile_observation_scope,
      observation_count = profile_observation_count,
      observation_since = profile_observation_since,
      episode_gap_hours = profile_episode_gap_hours
    ),
    error = function(error) list(
      profile = data.frame(), interval = NULL,
      warning = paste(
        "Dense post-dose profile could not be generated:",
        conditionMessage(error)
      )
    )
  )
  current_endpoint <- tryCatch(
    lator_endpoint_evaluate(endpoint, fit$predictions, patient = patient),
    error = function(error) list(error = conditionMessage(error))
  )
  input_hashes <- .lator_assessment_input_hashes(patient, analyte)
  assessment <- structure(list(
    schema = "liberator.assessment", version = 1L, assessment_id = .lator_id("assessment"),
    patient_id = patient$patient_id, patient_revision = patient$revision,
    created_at = .lator_now(), cutoff = cutoff, mode = mode, analyte = analyte,
    evidence_hash = .lator_hash(.lator_active_events(patient, cutoff = cutoff)),
    medication_hash = input_hashes$medication_hash,
    tdm_hash = input_hashes$tdm_hash,
    patient_context_hash = input_hashes$patient_context_hash,
    model_hash = .lator_hash(model), endpoint_hash = .lator_hash(endpoint),
    policy_hash = .lator_hash(covariate_policies), endpoint = endpoint,
    model_provenance = attr(model, "library_provenance", exact = TRUE) %||% list(),
    eta = fit$eta, eta_covariance = fit$eta_covariance, eta_trajectory = trajectory,
    individual_parameters = .lator_individual_parameters(fit),
    individual_profile = dense_profile$profile,
    individual_profile_interval = dense_profile$interval,
    predictions = fit$predictions, data = fit$data, covariate_policies = covariate_policies,
    profile_observation_scope = profile_observation_scope,
    profile_observation_count = as.integer(profile_observation_count),
    profile_observation_since = profile_observation_since,
    profile_episode_gap_hours = profile_episode_gap_hours,
    covariate_evidence = prepared$evidence,
    endpoint_evaluation = current_endpoint, convergence = fit$convergence,
    warnings = unique(c(
      prepared$warnings,
      .lator_dose_context_warnings(patient, analyte, cutoff),
      if (nzchar(dense_profile$warning %||% "")) dense_profile$warning
    )), diagnostics = c(fit$diagnostics, list(
      gradient_max = if (length(fit$gradient)) max(abs(fit$gradient)) else NA_real_,
      profile_grid_step_hours = 0.25,
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
