#' Construct a clinically feasible regimen grid
#'
#' @param amounts Dose amounts.
#' @param intervals Dose intervals in hours.
#' @param routes Route labels.
#' @param infusion_durations Infusion durations in hours; zero is an
#'   instantaneous input to the model's dose compartment.
#' @param loading_doses Optional loading amounts; `NA` means no separate load.
#' @param horizon Evaluation horizon in hours.
#' @return Candidate-regimen data frame.
#' @export
lator_regimen_candidates <- function(amounts, intervals, routes = "oral",
                                     infusion_durations = 0,
                                     loading_doses = NA_real_, horizon = 168) {
  amounts <- as.numeric(amounts); intervals <- as.numeric(intervals)
  infusion_durations <- as.numeric(infusion_durations)
  loading_doses <- as.numeric(loading_doses); horizon <- .lator_number(horizon, "horizon", positive = TRUE)
  if (!length(amounts) || any(!is.finite(amounts) | amounts <= 0)) .lator_stop("Dose amounts must be positive.")
  if (!length(intervals) || any(!is.finite(intervals) | intervals <= 0)) .lator_stop("Dose intervals must be positive.")
  if (!length(infusion_durations) || any(!is.finite(infusion_durations) | infusion_durations < 0)) {
    .lator_stop("Infusion durations must be non-negative.")
  }
  if (!length(loading_doses) || any(!is.na(loading_doses) & (!is.finite(loading_doses) | loading_doses <= 0))) {
    .lator_stop("Loading doses must be positive or NA.")
  }
  routes <- as.character(routes)
  if (!length(routes) || any(!nzchar(trimws(routes)))) .lator_stop("Routes must be non-empty.")
  output <- expand.grid(
    amount = unique(amounts), interval = unique(intervals), route = unique(routes),
    infusion_duration = unique(infusion_durations), loading_dose = unique(loading_doses),
    stringsAsFactors = FALSE
  )
  output$horizon <- horizon
  output$candidate_id <- sprintf("REG%03d", seq_len(nrow(output)))
  output[, c("candidate_id", "amount", "interval", "route", "infusion_duration", "loading_dose", "horizon")]
}

.lator_sample_mvn <- function(mean, covariance, n) {
  mean <- as.numeric(mean); covariance <- as.matrix(covariance)
  if (!length(mean)) return(matrix(numeric(), n, 0L))
  if (!identical(dim(covariance), c(length(mean), length(mean))) ||
      any(!is.finite(covariance))) {
    .lator_stop("ETA covariance must be a finite square matrix matching the ETA mean.")
  }
  covariance <- (covariance + t(covariance)) / 2
  eig <- eigen(covariance, symmetric = TRUE)
  scale <- max(1, max(abs(eig$values)))
  if (min(eig$values) < -sqrt(.Machine$double.eps) * scale) {
    .lator_stop(
      "ETA covariance is not positive semidefinite; regimen uncertainty ",
      "simulation was stopped instead of silently truncating negative eigenvalues."
    )
  }
  root <- eig$vectors %*% diag(sqrt(pmax(eig$values, 0)), length(mean))
  matrix(stats::rnorm(n * length(mean)), n, length(mean)) %*% t(root) +
    matrix(mean, n, length(mean), byrow = TRUE)
}

.lator_candidate_future <- function(candidate, start_time, grid_step, model) {
  end <- start_time + candidate$horizon
  dose_times <- seq(start_time, end - sqrt(.Machine$double.eps), by = candidate$interval)
  doses <- data.frame(
    TIME = dose_times, EVID = 1L, AMT = candidate$amount,
    RATE = if (candidate$infusion_duration > 0) candidate$amount / candidate$infusion_duration else 0,
    CMT = model$DOSECMP, II = 0, SS = 0L, ADDL = 0L, stringsAsFactors = FALSE
  )
  if (is.finite(candidate$loading_dose)) {
    doses <- doses[doses$TIME > start_time, , drop = FALSE]
    doses <- rbind(data.frame(
      TIME = start_time, EVID = 1L, AMT = candidate$loading_dose,
      RATE = if (candidate$infusion_duration > 0) candidate$loading_dose / candidate$infusion_duration else 0,
      CMT = model$DOSECMP, II = 0, SS = 0L, ADDL = 0L
    ), doses)
  }
  grid <- seq(start_time, end, by = grid_step)
  if (!length(grid) || utils::tail(grid, 1L) < end) grid <- c(grid, end)
  observations <- data.frame(
    TIME = sort(unique(grid)), EVID = 0L, AMT = 0,
    RATE = 0, CMT = model$OBSCMP, II = 0, SS = 0L, ADDL = 0L,
    stringsAsFactors = FALSE
  )
  rbind(doses, observations)
}

.lator_candidate_steady_state <- function(candidate, start_time, grid_step,
                                          model) {
  end <- start_time + candidate$interval
  grid <- seq(start_time, end, by = grid_step)
  if (!length(grid) || utils::tail(grid, 1L) < end) grid <- c(grid, end)
  dose <- data.frame(
    TIME = start_time, EVID = 1L, AMT = candidate$amount,
    RATE = if (candidate$infusion_duration > 0) {
      candidate$amount / candidate$infusion_duration
    } else 0,
    CMT = model$DOSECMP, II = candidate$interval, SS = 1L, ADDL = 0L,
    stringsAsFactors = FALSE
  )
  observations <- data.frame(
    TIME = sort(unique(grid)), EVID = 0L, AMT = 0, RATE = 0,
    CMT = model$OBSCMP, II = 0, SS = 0L, ADDL = 0L,
    stringsAsFactors = FALSE
  )
  rbind(dose, observations)
}

.lator_candidate_covariates <- function(data, candidate, model,
                                        future_only = TRUE) {
  data <- as.data.frame(data)
  rows <- if (isTRUE(future_only) && ".LATOR_ROLE" %in% names(data)) {
    data$.LATOR_ROLE %in% "future"
  } else rep(TRUE, nrow(data))
  if (!any(rows)) return(data)
  covariates <- toupper(as.character(model$COVARIATES %||% character()))
  columns <- toupper(names(data))
  assign_value <- function(candidates, value) {
    requested <- intersect(candidates, covariates)
    for (name in requested) {
      column <- match(name, columns)
      if (!is.na(column)) data[[column]][rows] <<- value
    }
  }
  daily <- as.numeric(candidate$amount) * 24 / as.numeric(candidate$interval)
  assign_value(c("DAILY_DOSE", "TOTAL_DAILY_DOSE", "TDD"), daily)
  weight_column <- match("WT", columns)
  if (!is.na(weight_column)) {
    weight <- suppressWarnings(as.numeric(data[[weight_column]][rows]))
    per_kg <- daily / weight
    per_kg[!is.finite(per_kg) | weight <= 0] <- NA_real_
    assign_value(c("DOSE_MG_KG_DAY", "DAILY_DOSE_MG_KG"), per_kg)
  }
  data
}

.lator_replicate_dataset <- function(data, n, prefix = "SIM") {
  drop <- intersect(c(".ID_INDEX", ".source_row", ".generated", ".sort_priority", ".OCC_INDEX"), names(data))
  data <- as.data.frame(data[, setdiff(names(data), drop), drop = FALSE])
  pieces <- lapply(seq_len(n), function(index) {
    copy <- data
    copy$ID <- paste0(prefix, index)
    copy$SIM <- index
    copy
  })
  do.call(rbind, pieces)
}

.lator_bind_datasets <- function(...) {
  pieces <- list(...)
  columns <- unique(unlist(lapply(pieces, names), use.names = FALSE))
  pieces <- lapply(pieces, function(piece) {
    missing <- setdiff(columns, names(piece))
    for (name in missing) piece[[name]] <- NA
    piece[, columns, drop = FALSE]
  })
  do.call(rbind, pieces)
}

.lator_prediction_cycle_statistics <- function(predictions, value_column = NULL) {
  predictions <- as.data.frame(predictions)
  columns <- .lator_prediction_columns(predictions, value_column)
  predictions$.time <- suppressWarnings(as.numeric(predictions[[columns$time]]))
  predictions$.value <- suppressWarnings(as.numeric(predictions[[columns$value]]))
  predictions$.sim <- if ("SIM" %in% names(predictions)) {
    as.integer(predictions$SIM)
  } else 1L
  predictions <- predictions[
    is.finite(predictions$.time) & is.finite(predictions$.value), ,
    drop = FALSE
  ]
  if (!nrow(predictions)) return(data.frame())
  durations <- vapply(split(predictions$.time, predictions$.sim), function(x) diff(range(x)), numeric(1))
  duration <- stats::median(durations[is.finite(durations) & durations > 0])
  if (!is.finite(duration)) duration <- NA_real_
  nca_input <- data.frame(
    SIM = predictions$.sim, TIME = predictions$.time,
    CONC = predictions$.value
  )
  nca <- tryCatch(
    LibeRation::nm_nca(
      nca_input, time = "TIME", concentration = "CONC", id = "SIM",
      tau = duration, method = "lin_up_log_down", engine = "native"
    )$results,
    error = function(e) NULL
  )
  if (!is.null(nca) && nrow(nca)) {
    return(data.frame(
      SIM = as.integer(nca$SIM), mean_css = as.numeric(nca$CAVG),
      trough = as.numeric(nca$CMIN), peak = as.numeric(nca$CMAX),
      fluctuation_percent = as.numeric(nca$FLUCTUATION_PERCENT),
      stringsAsFactors = FALSE
    ))
  }
  # Retain a defensive R fallback for profiles that cannot meet NCA input
  # requirements (for example a single simulated time point).
  rows <- lapply(split(predictions, predictions$.sim), function(item) {
    item <- item[order(item$.time), , drop = FALSE]
    span <- diff(range(item$.time))
    mean_css <- if (is.finite(span) && span > 0) .lator_trapz(item$.time, item$.value) / span else utils::tail(item$.value, 1L)
    peak <- max(item$.value); trough <- min(item$.value)
    data.frame(SIM = item$.sim[[1L]], mean_css = mean_css, trough = trough,
               peak = peak, fluctuation_percent = if (abs(mean_css) > sqrt(.Machine$double.eps)) 100 * (peak - trough) / abs(mean_css) else NA_real_)
  })
  do.call(rbind, rows)
}

.lator_metric_summary <- function(metrics, probs = c(0.05, 0.5, 0.95),
                                  target = NULL) {
  metrics <- as.data.frame(metrics)
  summarise <- function(name) {
    values <- suppressWarnings(as.numeric(metrics[[name]]))
    values <- values[is.finite(values)]
    if (!length(values)) {
      return(list(lower = NA_real_, median = NA_real_, upper = NA_real_))
    }
    quantiles <- stats::quantile(
      values, probs = probs, names = FALSE, type = 8, na.rm = TRUE
    )
    list(
      lower = unname(quantiles[[1L]]),
      median = unname(quantiles[[2L]]),
      upper = unname(quantiles[[3L]])
    )
  }
  result <- list(
    mean_css = summarise("mean_css"),
    trough = summarise("trough"),
    peak = summarise("peak"),
    fluctuation_percent = summarise("fluctuation_percent")
  )
  if (!is.null(target) && all(is.finite(c(target$lower, target$upper)))) {
    result$whole_cycle_in_range_probability <- mean(
      metrics$trough >= target$lower & metrics$peak <= target$upper,
      na.rm = TRUE
    )
  } else {
    result$whole_cycle_in_range_probability <- NA_real_
  }
  result
}

.lator_steady_state_endpoint <- function(endpoint) {
  endpoint <- lator_endpoint_validate(endpoint)
  if (identical(endpoint$kind, "multi_endpoint")) {
    return(all(vapply(
      endpoint$rules$components,
      function(component) .lator_steady_state_endpoint(component$endpoint),
      logical(1)
    )))
  }
  endpoint$kind %in% c(
    "therapeutic_range", "trough_range", "auc_range", "auc_mic_range",
    "peak_mic_safety", "fraction_time_above_threshold", "time_in_range"
  )
}

.lator_target_range <- function(endpoint) {
  endpoint <- lator_endpoint_validate(endpoint)
  if (identical(endpoint$kind, "multi_endpoint")) {
    primary <- Filter(
      function(component) identical(component$role, "primary"),
      endpoint$rules$components
    )
    if (!length(primary)) return(NULL)
    return(.lator_target_range(primary[[1L]]$endpoint))
  }
  if (!endpoint$kind %in% c("therapeutic_range", "trough_range")) return(NULL)
  list(
    lower = as.numeric(endpoint$rules$lower),
    upper = as.numeric(endpoint$rules$upper),
    unit = endpoint$unit
  )
}

.lator_regimen_endpoint_evaluate <- function(
    endpoint, transition_predictions, steady_predictions, patient,
    transition_interval, steady_interval, value_column = NULL) {
  endpoint <- lator_endpoint_validate(endpoint)
  if (!identical(endpoint$kind, "multi_endpoint")) {
    if (.lator_steady_state_endpoint(endpoint)) {
      return(lator_endpoint_evaluate(
        endpoint, steady_predictions, patient = patient,
        interval = steady_interval, value_column = value_column
      ))
    }
    return(lator_endpoint_evaluate(
      endpoint, transition_predictions, patient = patient,
      interval = transition_interval, value_column = value_column
    ))
  }
  evaluations <- lapply(endpoint$rules$components, function(component) {
    nested <- component$endpoint
    if (.lator_steady_state_endpoint(nested)) {
      lator_endpoint_evaluate(
        nested, steady_predictions, patient = patient,
        interval = steady_interval, value_column = value_column
      )
    } else {
      lator_endpoint_evaluate(
        nested, transition_predictions, patient = patient,
        interval = transition_interval, value_column = value_column
      )
    }
  })
  .lator_endpoint_combine_evaluations(endpoint, evaluations)
}

.lator_pareto_flags <- function(criteria, eligible, tolerance = 1e-12) {
  criteria <- as.matrix(criteria)
  eligible <- as.logical(eligible)
  result <- rep(FALSE, nrow(criteria))
  candidates <- which(eligible & apply(criteria, 1L, function(row) {
    all(is.finite(row))
  }))
  for (index in candidates) {
    others <- setdiff(candidates, index)
    dominated <- any(vapply(others, function(other) {
      all(criteria[other, ] <= criteria[index, ] + tolerance) &&
        any(criteria[other, ] < criteria[index, ] - tolerance)
    }, logical(1)))
    result[[index]] <- !dominated
  }
  result
}

.lator_last_complete_cycle <- function(predictions, candidate, start_time) {
  end <- start_time + as.numeric(candidate$horizon)
  interval <- as.numeric(candidate$interval)
  dose_times <- seq(
    start_time, end - sqrt(.Machine$double.eps), by = interval
  )
  complete <- dose_times[dose_times + interval <= end + sqrt(.Machine$double.eps)]
  if (!length(complete)) return(data.frame())
  cycle_start <- utils::tail(complete, 1L)
  predictions[
    predictions$TIME >= cycle_start &
      predictions$TIME <= cycle_start + interval, ,
    drop = FALSE
  ]
}

.lator_horizon_convergence <- function(transition_metrics, steady_metrics,
                                       tolerance = 0.05,
                                       profile_type = "time_course") {
  if (identical(profile_type, "steady_state_mean_only")) {
    return(list(
      evaluable = FALSE, probability = NA_real_,
      median_relative_difference = NA_real_,
      tolerance = tolerance,
      status = "mean-only model; transition kinetics are unavailable"
    ))
  }
  joined <- merge(
    transition_metrics[, c("SIM", "mean_css"), drop = FALSE],
    steady_metrics[, c("SIM", "mean_css"), drop = FALSE],
    by = "SIM", suffixes = c("_horizon", "_steady_state")
  )
  if (!nrow(joined)) {
    return(list(
      evaluable = FALSE, probability = NA_real_,
      median_relative_difference = NA_real_, tolerance = tolerance,
      status = "forecast horizon is shorter than one complete dosing cycle"
    ))
  }
  difference <- abs(
    joined$mean_css_horizon - joined$mean_css_steady_state
  ) / pmax(abs(joined$mean_css_steady_state), sqrt(.Machine$double.eps))
  probability <- mean(difference <= tolerance, na.rm = TRUE)
  list(
    evaluable = TRUE, probability = probability,
    median_relative_difference = stats::median(difference, na.rm = TRUE),
    tolerance = tolerance,
    status = if (is.finite(probability) && probability >= 0.9) {
      "forecast horizon is close to periodic steady state"
    } else {
      "forecast horizon has not reliably reached periodic steady state"
    }
  )
}

#' Compare candidate regimens under conditional individual uncertainty
#'
#' The complete dosing history is replayed for every conditional ETA draw so that
#' accumulated compartment amounts are retained. Parameter uncertainty is
#' evaluated in one batched C++ simulation call per candidate. These draws use
#' the individual's Laplace approximation conditional on the selected model and
#' fitted population parameters; they do not include population-parameter or
#' model-structure uncertainty.
#'
#' @param assessment A completed `lator_assessment`.
#' @param patient The corresponding patient timeline.
#' @param candidates Candidate data frame from [lator_regimen_candidates()].
#' @param endpoint Endpoint or versioned multi-endpoint objective to optimise;
#'   defaults to the assessment endpoint. Multi-endpoint components are
#'   evaluated on shared conditional ETA draws.
#' @param nsim Number of conditional ETA draws.
#' @param grid_step Prediction-grid spacing in hours.
#' @param start_time First candidate dose time; defaults just after the latest
#'   historical event visible to the assessment.
#' @param residual Include residual observation variability in attainment probabilities.
#' @param max_daily_dose Optional hard feasibility constraint.
#' @param max_single_dose Optional hard feasibility constraint.
#' @param dose_burden_weight Small optional tie-breaker applied to daily dose.
#' @param seed Reproducible seed.
#' @param n_cores Simulation cores used within LibeRation.
#' @return A ranked result with summaries and per-candidate trajectories.
#'   Multi-endpoint results also include component and joint attainment,
#'   normalized expected utility, hard-constraint status, and Pareto status.
#' @export
lator_regimen_optimise <- function(assessment, patient, candidates,
                                   endpoint = assessment$endpoint, nsim = 200L,
                                   grid_step = 0.25, start_time = NULL,
                                   residual = FALSE, max_daily_dose = Inf,
                                   max_single_dose = Inf, dose_burden_weight = 0,
                                   seed = NULL, n_cores = 1L) {
  if (!inherits(assessment, "lator_assessment")) .lator_stop("`assessment` must be a LibeRator assessment.")
  patient <- .lator_validate_patient(patient); endpoint <- lator_endpoint_validate(endpoint)
  candidates <- as.data.frame(candidates, stringsAsFactors = FALSE)
  required <- c("candidate_id", "amount", "interval", "route", "infusion_duration", "loading_dose", "horizon")
  if (!all(required %in% names(candidates)) || !nrow(candidates)) .lator_stop("Candidate table is invalid.")
  nsim <- as.integer(nsim); n_cores <- as.integer(n_cores)
  if (is.na(nsim) || nsim < 1L) .lator_stop("`nsim` must be positive.")
  if (is.na(n_cores) || n_cores < 1L) .lator_stop("`n_cores` must be positive.")
  grid_step <- .lator_number(grid_step, "grid_step", positive = TRUE)
  if (!is.null(seed)) set.seed(as.integer(seed))
  visible_events <- .lator_active_events(patient, cutoff = assessment$cutoff)
  latest <- if (length(visible_events)) max(vapply(visible_events, `[[`, numeric(1), "time")) else 0
  start_time <- .lator_number(start_time %||% (latest + sqrt(.Machine$double.eps)), "start_time")
  model <- assessment$model
  state_times <- assessment$eta_trajectory$start_time[is.finite(assessment$eta_trajectory$start_time)]
  eta_samples <- .lator_sample_mvn(assessment$eta, assessment$eta_covariance, nsim)
  summaries <- vector("list", nrow(candidates)); trajectories <- vector("list", nrow(candidates))
  target <- .lator_target_range(endpoint)
  profile_type <- .lator_model_profile_type(model)

  for (index in seq_len(nrow(candidates))) {
    candidate <- candidates[index, , drop = FALSE]
    daily <- candidate$amount * 24 / candidate$interval
    feasible <- candidate$amount <= max_single_dose && daily <= max_daily_dose
    if (!feasible) {
      summaries[[index]] <- data.frame(
        candidate_id = candidate$candidate_id, feasible = FALSE, daily_dose = daily,
        attainment_probability = NA_real_, median_metric = NA_real_, median_score = Inf,
        joint_attainment_probability = NA_real_,
        primary_attainment_probability = NA_real_,
        expected_utility = NA_real_, hard_constraints_pass = FALSE,
        steady_state_mean = NA_real_, steady_state_trough = NA_real_,
        steady_state_peak = NA_real_, whole_cycle_in_range_probability = NA_real_,
        horizon_steady_state_probability = NA_real_,
        profile_type = profile_type,
        objective = Inf, stringsAsFactors = FALSE
      )
      next
    }
    future <- .lator_candidate_future(candidate, start_time, grid_step, model)
    prepared_transition <- .lator_patient_dataset(
      patient, model, assessment$analyte, cutoff = assessment$cutoff,
      covariate_policies = assessment$covariate_policies %||% list(),
      dynamic = identical(assessment$mode, "dynamic"),
      state_times = state_times, include_future = future
    )
    transition_data <- .lator_candidate_covariates(
      prepared_transition$data, candidate, model
    )
    steady_state <- .lator_candidate_steady_state(
      candidate, start_time, grid_step, model
    )
    prepared_steady <- .lator_patient_dataset(
      patient, model, assessment$analyte, cutoff = assessment$cutoff,
      covariate_policies = assessment$covariate_policies %||% list(),
      dynamic = identical(assessment$mode, "dynamic"),
      state_times = state_times, include_future = steady_state
    )
    steady_data <- prepared_steady$data[
      prepared_steady$data$.LATOR_ROLE %in% "future", , drop = FALSE
    ]
    steady_data <- .lator_candidate_covariates(
      steady_data, candidate, model, future_only = FALSE
    )
    transition_replicated <- .lator_replicate_dataset(
      transition_data, nsim, "TRANS"
    )
    steady_replicated <- .lator_replicate_dataset(
      steady_data, nsim, "STEADY"
    )
    replicated <- .lator_bind_datasets(
      transition_replicated, steady_replicated
    )
    predictions <- LibeRation::nm_simulate(
      model, replicated, eta = rbind(eta_samples, eta_samples),
      residual = isTRUE(residual),
      nsim = 1L, n_cores = n_cores
    )
    endpoint_value_column <- if (isTRUE(residual)) "DV" else "IPRED"
    prediction_id <- as.character(predictions$ID)
    transition_predictions <- predictions[
      grepl("^TRANS", prediction_id) &
        predictions$TIME >= start_time & predictions$EVID == 0L, ,
      drop = FALSE
    ]
    transition_predictions$SIM <- as.integer(sub(
      "^TRANS", "", as.character(transition_predictions$ID)
    ))
    steady_predictions <- predictions[
      grepl("^STEADY", prediction_id) & predictions$EVID == 0L, ,
      drop = FALSE
    ]
    steady_predictions$SIM <- as.integer(sub(
      "^STEADY", "", as.character(steady_predictions$ID)
    ))
    transition_evaluation <- lator_endpoint_evaluate(
      endpoint, transition_predictions, patient = patient,
      interval = c(start_time, start_time + candidate$horizon),
      value_column = endpoint_value_column
    )
    steady_evaluation <- lator_endpoint_evaluate(
      endpoint, steady_predictions, patient = patient,
      interval = c(start_time, start_time + candidate$interval),
      value_column = endpoint_value_column
    )
    steady_metrics <- .lator_prediction_cycle_statistics(
      steady_predictions, value_column = endpoint_value_column
    )
    steady_summary <- .lator_metric_summary(
      steady_metrics, target = target
    )
    if (identical(profile_type, "steady_state_mean_only")) {
      unavailable <- list(
        lower = NA_real_, median = NA_real_, upper = NA_real_
      )
      steady_summary$trough <- unavailable
      steady_summary$peak <- unavailable
      steady_summary$fluctuation_percent <- unavailable
      steady_summary$whole_cycle_in_range_probability <- NA_real_
    }
    last_cycle <- .lator_last_complete_cycle(
      transition_predictions, candidate, start_time
    )
    last_cycle_metrics <- .lator_prediction_cycle_statistics(
      last_cycle, value_column = endpoint_value_column
    )
    convergence <- .lator_horizon_convergence(
      last_cycle_metrics, steady_metrics, profile_type = profile_type
    )
    decision_evaluation <- .lator_regimen_endpoint_evaluate(
      endpoint, transition_predictions, steady_predictions, patient,
      transition_interval = c(
        start_time, start_time + candidate$horizon
      ),
      steady_interval = c(
        start_time, start_time + candidate$interval
      ), value_column = endpoint_value_column
    )
    is_multi <- identical(endpoint$kind, "multi_endpoint")
    expected_utility <- if (is_multi) {
      decision_evaluation$expected_utility
    } else {
      exp(-max(0, decision_evaluation$median_score))
    }
    hard_constraints_pass <- if (is_multi) {
      isTRUE(decision_evaluation$hard_constraints_pass)
    } else TRUE
    objective <- if (is_multi) {
      1 - expected_utility
    } else decision_evaluation$median_score
    objective <- objective +
      dose_burden_weight * daily
    summaries[[index]] <- data.frame(
      candidate_id = candidate$candidate_id, feasible = TRUE, daily_dose = daily,
      attainment_probability = decision_evaluation$attainment_probability,
      median_metric = decision_evaluation$median_metric,
      median_score = decision_evaluation$median_score,
      joint_attainment_probability =
        decision_evaluation$joint_attainment_probability %||%
          decision_evaluation$attainment_probability,
      primary_attainment_probability =
        decision_evaluation$primary_attainment_probability %||%
          decision_evaluation$attainment_probability,
      expected_utility = expected_utility,
      hard_constraints_pass = hard_constraints_pass,
      steady_state_mean = steady_summary$mean_css$median,
      steady_state_trough = steady_summary$trough$median,
      steady_state_peak = steady_summary$peak$median,
      whole_cycle_in_range_probability =
        steady_summary$whole_cycle_in_range_probability,
      horizon_steady_state_probability = convergence$probability,
      profile_type = profile_type,
      objective = objective, stringsAsFactors = FALSE
    )
    trajectories[[index]] <- list(
      candidate = candidate,
      predictions = transition_predictions,
      endpoint_value_column = endpoint_value_column,
      evaluation = decision_evaluation,
      transition_evaluation = transition_evaluation,
      steady_state = list(
        predictions = steady_predictions,
        evaluation = steady_evaluation,
        metrics = steady_metrics,
        summary = steady_summary,
        profile_type = profile_type
      ),
      horizon_convergence = convergence
    )
  }
  summary <- merge(candidates, do.call(rbind, summaries), by = "candidate_id", all.x = TRUE, sort = FALSE)
  summary$decision_eligible <- summary$feasible &
    summary$hard_constraints_pass
  summary$pareto_optimal <- FALSE
  if (identical(endpoint$kind, "multi_endpoint")) {
    component_ids <- vapply(
      endpoint$rules$components, `[[`, character(1), "component_id"
    )
    criteria <- matrix(
      Inf, nrow = nrow(summary), ncol = length(component_ids),
      dimnames = list(summary$candidate_id, component_ids)
    )
    trajectory_ids <- vapply(trajectories, function(item) {
      if (is.null(item)) return(NA_character_)
      as.character(item$candidate$candidate_id[[1L]])
    }, character(1))
    for (row in seq_len(nrow(summary))) {
      location <- match(summary$candidate_id[[row]], trajectory_ids)
      if (is.na(location) || is.null(trajectories[[location]])) next
      component_result <- trajectories[[location]]$evaluation$components
      criteria[row, ] <- 1 - component_result$expected_utility[
        match(component_ids, component_result$component_id)
      ]
    }
    if (dose_burden_weight > 0) {
      criteria <- cbind(
        criteria,
        dose_burden = summary$daily_dose /
          max(summary$daily_dose[is.finite(summary$daily_dose)])
      )
    }
    summary$pareto_optimal <- .lator_pareto_flags(
      criteria, summary$decision_eligible
    )
  } else {
    eligible <- which(summary$decision_eligible)
    if (length(eligible)) {
      best <- eligible[[which.min(summary$objective[eligible])]]
      summary$pareto_optimal[[best]] <- TRUE
    }
  }
  summary <- summary[order(
    !summary$decision_eligible, summary$objective,
    -summary$joint_attainment_probability, summary$daily_dose
  ), , drop = FALSE]
  summary$rank <- seq_len(nrow(summary))
  rownames(summary) <- NULL
  structure(list(
    schema = "liberator.regimen-comparison",
    version = if (identical(endpoint$kind, "multi_endpoint")) 2L else 1L,
    assessment_id = assessment$assessment_id, endpoint = endpoint,
    summary = summary, trajectories = trajectories,
    start_time = start_time,
    uncertainty = list(
      nsim = nsim, residual = isTRUE(residual), seed = seed,
      scope = if (isTRUE(residual)) {
        "conditional ETA Laplace approximation plus residual observation variability"
      } else "conditional ETA Laplace approximation"
    ),
    generated_at = .lator_now(), research_only = TRUE
  ), class = "lator_regimen_comparison")
}

#' Create an explicit future prediction for a selected regimen
#'
#' Regimen comparison already simulates every feasible candidate under the
#' patient's conditional ETA uncertainty. This function promotes one of those
#' candidate trajectories into a separate, auditable forecast artifact instead
#' of repeating the expensive simulation or treating the highest-ranked row as
#' an automatic dosing decision.
#'
#' @param comparison Result from [lator_regimen_optimise()].
#' @param candidate_id Candidate selected by the user.
#' @param probs Three ordered conditional probabilities used for the lower
#'   interval, median, and upper interval. The interval reflects uncertainty
#'   in the individual's conditional ETA approximation. Residual variability is
#'   included only when requested during regimen comparison; population-parameter
#'   and model-structure uncertainty are excluded. It is not a confidence
#'   interval for a population mean.
#' @return A `lator_future_prediction` containing pointwise conditional
#'   prediction intervals, their uncertainty definition, the selected regimen,
#'   its endpoint evaluation, and native-unit numerical outcome intervals for
#'   every endpoint component.
#' @export
lator_regimen_predict <- function(comparison, candidate_id,
                                  probs = c(0.05, 0.5, 0.95)) {
  if (!inherits(comparison, "lator_regimen_comparison") ||
      !identical(comparison$schema, "liberator.regimen-comparison")) {
    .lator_stop("`comparison` must be created by lator_regimen_optimise().")
  }
  candidate_id <- .lator_scalar(candidate_id, "candidate_id", max_chars = 128L)
  probs <- as.numeric(probs)
  if (length(probs) != 3L || any(!is.finite(probs)) ||
      any(probs <= 0 | probs >= 1) || any(diff(probs) <= 0)) {
    .lator_stop("`probs` must contain three increasing probabilities between zero and one.")
  }
  summary_row <- comparison$summary[
    as.character(comparison$summary$candidate_id) == candidate_id, , drop = FALSE
  ]
  if (nrow(summary_row) != 1L) .lator_stop("Unknown regimen candidate: ", candidate_id, ".")
  if (!isTRUE(summary_row$feasible[[1L]])) .lator_stop("The selected regimen is not feasible.")
  if ("decision_eligible" %in% names(summary_row) &&
      !isTRUE(summary_row$decision_eligible[[1L]])) {
    .lator_stop(
      "The selected regimen does not satisfy the endpoint set's hard ",
      "conditional-draw probability constraints."
    )
  }

  trajectory_ids <- vapply(comparison$trajectories, function(item) {
    if (is.null(item) || is.null(item$candidate)) return(NA_character_)
    as.character(item$candidate$candidate_id[[1L]])
  }, character(1))
  location <- match(candidate_id, trajectory_ids)
  if (is.na(location) || is.null(comparison$trajectories[[location]])) {
    .lator_stop("No simulated future trajectory is available for ", candidate_id, ".")
  }
  trajectory <- comparison$trajectories[[location]]
  predictions <- as.data.frame(trajectory$predictions)
  value_column <- trajectory$endpoint_value_column %||%
    if (isTRUE(comparison$uncertainty$residual)) "DV" else "IPRED"
  columns <- .lator_prediction_columns(predictions, value_column)
  time <- as.numeric(predictions[[columns$time]])
  prediction <- as.numeric(predictions[[columns$value]])
  keep <- is.finite(time) & is.finite(prediction)
  if (!any(keep)) .lator_stop("The selected regimen has no finite future predictions.")
  groups <- split(prediction[keep], time[keep])
  times <- as.numeric(names(groups))
  ordering <- order(times)
  quantiles <- do.call(rbind, lapply(groups, function(values) {
    result <- stats::quantile(values, probs = probs, names = FALSE, type = 8, na.rm = TRUE)
    c(lower = result[[1L]], median = result[[2L]], upper = result[[3L]],
      mean = mean(values, na.rm = TRUE), draws = length(values))
  }))
  forecast <- data.frame(time = times, quantiles, row.names = NULL)
  forecast <- forecast[ordering, , drop = FALSE]

  endpoint <- comparison$endpoint
  target <- .lator_target_range(endpoint)
  endpoint_outcomes <- .lator_endpoint_outcome_summary(
    endpoint, trajectory$evaluation, probs = probs
  )
  steady_predictions <- as.data.frame(
    trajectory$steady_state$predictions %||% data.frame()
  )
  steady_forecast <- data.frame()
  if (nrow(steady_predictions)) {
    steady_columns <- .lator_prediction_columns(
      steady_predictions, value_column
    )
    steady_time <- as.numeric(steady_predictions[[steady_columns$time]])
    steady_value <- as.numeric(steady_predictions[[steady_columns$value]])
    steady_keep <- is.finite(steady_time) & is.finite(steady_value)
    steady_groups <- split(steady_value[steady_keep], steady_time[steady_keep])
    steady_times <- as.numeric(names(steady_groups))
    steady_quantiles <- do.call(rbind, lapply(
      steady_groups, function(values) {
        result <- stats::quantile(
          values, probs = probs, names = FALSE, type = 8, na.rm = TRUE
        )
        c(
          lower = result[[1L]], median = result[[2L]],
          upper = result[[3L]], mean = mean(values, na.rm = TRUE),
          draws = length(values)
        )
      }
    ))
    steady_forecast <- data.frame(
      time = steady_times - min(steady_times), steady_quantiles,
      row.names = NULL
    )
    steady_forecast <- steady_forecast[
      order(steady_forecast$time), , drop = FALSE
    ]
  }
  structure(list(
    schema = "liberator.future-prediction", version = 2L,
    prediction_id = .lator_id("forecast"),
    assessment_id = comparison$assessment_id,
    candidate_id = candidate_id,
    regimen = summary_row,
    forecast = forecast,
    interval_probabilities = probs,
    uncertainty = list(
      interval_type = "pointwise_conditional_prediction_interval",
      sources = if (isTRUE(comparison$uncertainty$residual)) {
        c("conditional_eta_laplace_approximation", "residual_observation_variability")
      } else "conditional_eta_laplace_approximation",
      residual_measurement_variability = isTRUE(comparison$uncertainty$residual),
      population_parameter_uncertainty = FALSE,
      model_structure_uncertainty = FALSE,
      interpretation = paste(
        "Conditional on the fitted population parameters and selected model;",
        "this is not a full Bayesian posterior predictive interval."
      ),
      probabilities = probs
    ),
    target = target,
    endpoint = endpoint,
    evaluation = trajectory$evaluation,
    endpoint_outcomes = endpoint_outcomes,
    transition_evaluation = trajectory$transition_evaluation,
    steady_state_forecast = steady_forecast,
    steady_state = c(
      trajectory$steady_state[c("summary", "profile_type")],
      list(
        evaluation = trajectory$steady_state$evaluation,
        horizon_convergence = trajectory$horizon_convergence
      )
    ),
    generated_at = .lator_now(),
    research_only = TRUE
  ), class = "lator_future_prediction")
}
