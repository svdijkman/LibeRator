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
  covariance <- (covariance + t(covariance)) / 2
  eig <- eigen(covariance, symmetric = TRUE)
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
  observations <- data.frame(
    TIME = seq(start_time, end, by = grid_step), EVID = 0L, AMT = 0,
    RATE = 0, CMT = model$OBSCMP, II = 0, SS = 0L, ADDL = 0L,
    stringsAsFactors = FALSE
  )
  rbind(doses, observations)
}

.lator_replicate_dataset <- function(data, n) {
  drop <- intersect(c(".ID_INDEX", ".source_row", ".generated", ".sort_priority", ".OCC_INDEX"), names(data))
  data <- as.data.frame(data[, setdiff(names(data), drop), drop = FALSE])
  pieces <- lapply(seq_len(n), function(index) {
    copy <- data; copy$ID <- paste0("SIM", index); copy$SIM <- index; copy
  })
  do.call(rbind, pieces)
}

#' Compare candidate regimens under posterior uncertainty
#'
#' The complete dosing history is replayed for every posterior draw so that
#' accumulated compartment amounts are retained. Parameter uncertainty is
#' evaluated in one batched C++ simulation call per candidate.
#'
#' @param assessment A completed `lator_assessment`.
#' @param patient The corresponding patient timeline.
#' @param candidates Candidate data frame from [lator_regimen_candidates()].
#' @param endpoint Endpoint to optimise; defaults to the assessment endpoint.
#' @param nsim Number of posterior draws.
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

  for (index in seq_len(nrow(candidates))) {
    candidate <- candidates[index, , drop = FALSE]
    daily <- candidate$amount * 24 / candidate$interval
    feasible <- candidate$amount <= max_single_dose && daily <= max_daily_dose
    if (!feasible) {
      summaries[[index]] <- data.frame(
        candidate_id = candidate$candidate_id, feasible = FALSE, daily_dose = daily,
        attainment_probability = NA_real_, median_metric = NA_real_, median_score = Inf,
        objective = Inf, stringsAsFactors = FALSE
      )
      next
    }
    future <- .lator_candidate_future(candidate, start_time, grid_step, model)
    prepared <- .lator_patient_dataset(
      patient, model, assessment$analyte, cutoff = assessment$cutoff,
      covariate_policies = assessment$covariate_policies %||% list(),
      dynamic = identical(assessment$mode, "dynamic"),
      state_times = state_times, include_future = future
    )
    replicated <- .lator_replicate_dataset(prepared$data, nsim)
    predictions <- LibeRation::nm_simulate(
      model, replicated, eta = eta_samples, residual = isTRUE(residual),
      nsim = 1L, n_cores = n_cores
    )
    predictions$SIM <- as.integer(sub("^SIM", "", as.character(predictions$ID)))
    evaluation_rows <- predictions$TIME >= start_time & predictions$EVID == 0L
    evaluated <- lator_endpoint_evaluate(
      endpoint, predictions[evaluation_rows, , drop = FALSE], patient = patient,
      interval = c(start_time, start_time + candidate$horizon)
    )
    objective <- evaluated$median_score + dose_burden_weight * daily
    summaries[[index]] <- data.frame(
      candidate_id = candidate$candidate_id, feasible = TRUE, daily_dose = daily,
      attainment_probability = evaluated$attainment_probability,
      median_metric = evaluated$median_metric, median_score = evaluated$median_score,
      objective = objective, stringsAsFactors = FALSE
    )
    trajectories[[index]] <- list(candidate = candidate, predictions = predictions[evaluation_rows, , drop = FALSE],
                                  evaluation = evaluated)
  }
  summary <- merge(candidates, do.call(rbind, summaries), by = "candidate_id", all.x = TRUE, sort = FALSE)
  summary <- summary[order(!summary$feasible, summary$objective, -summary$attainment_probability, summary$daily_dose), , drop = FALSE]
  rownames(summary) <- NULL
  structure(list(
    schema = "liberator.regimen-comparison", version = 1L,
    assessment_id = assessment$assessment_id, endpoint = endpoint,
    summary = summary, trajectories = trajectories,
    start_time = start_time,
    uncertainty = list(nsim = nsim, residual = isTRUE(residual), seed = seed),
    generated_at = .lator_now(), research_only = TRUE
  ), class = "lator_regimen_comparison")
}

#' Create an explicit future prediction for a selected regimen
#'
#' Regimen comparison already simulates every feasible candidate under the
#' patient's posterior uncertainty. This function promotes one of those
#' candidate trajectories into a separate, auditable forecast artifact instead
#' of repeating the expensive simulation or treating the highest-ranked row as
#' an automatic dosing decision.
#'
#' @param comparison Result from [lator_regimen_optimise()].
#' @param candidate_id Candidate selected by the user.
#' @param probs Three ordered probabilities used for the lower interval,
#'   median, and upper interval.
#' @return A `lator_future_prediction` containing pointwise uncertainty bands,
#'   the selected regimen, and its endpoint evaluation.
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
  columns <- .lator_prediction_columns(predictions)
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
  target <- if (endpoint$kind %in% c("therapeutic_range", "trough_range")) {
    list(lower = endpoint$rules$lower, upper = endpoint$rules$upper,
         unit = endpoint$unit)
  } else NULL
  structure(list(
    schema = "liberator.future-prediction", version = 1L,
    prediction_id = .lator_id("forecast"),
    assessment_id = comparison$assessment_id,
    candidate_id = candidate_id,
    regimen = summary_row,
    forecast = forecast,
    interval_probabilities = probs,
    target = target,
    endpoint = endpoint,
    evaluation = trajectory$evaluation,
    generated_at = .lator_now(),
    research_only = TRUE
  ), class = "lator_future_prediction")
}
