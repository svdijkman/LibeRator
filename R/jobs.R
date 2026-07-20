#' Create a deidentified LibeRties individualisation or regimen job
#'
#' The caller supplies model-ready event data rather than a LibeRator patient
#' record. Workspace keys, patient catalogues, audit records, and direct
#' identifiers are therefore never included in a queue payload.
#'
#' @param type `"individualise"` or `"regimen"`.
#' @param model Serializable LibeRation model.
#' @param data Deidentified model-ready data. Regimen jobs require a
#'   `CANDIDATE` column.
#' @param arguments Named worker arguments.
#' @param label Job label.
#' @return A LibeRties job ready for a local or remote queue.
#' @export
lator_job <- function(type = c("individualise", "regimen"), model, data,
                      arguments = list(), label = NULL) {
  if (!requireNamespace("LibeRties", quietly = TRUE)) .lator_stop("Install LibeRties to create queue jobs.")
  type <- match.arg(type)
  if (!inherits(model, "nm_model")) .lator_stop("`model` must be a serializable LibeRation nm_model.")
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (any(grepl("NAME|DOB|ADDRESS|EMAIL|PHONE|NHS|MRN", toupper(names(data))))) {
    .lator_stop("Queue data appears to contain a direct-identifier column. Supply pseudonymous model data only.")
  }
  if (type == "regimen" && !"CANDIDATE" %in% names(data)) .lator_stop("Regimen queue data requires `CANDIDATE`.")
  LibeRties::ls_job(type, model, data, arguments, label)
}

#' Execute a typed LibeRator worker task
#'
#' This is the narrow entry point called by LibeRties workers. It accepts no
#' workspace object or encryption key.
#' @param type Worker task.
#' @param model Serializable model.
#' @param data Model-ready data.
#' @param arguments Named controls.
#' @export
lator_worker_task <- function(type, model, data, arguments = list()) {
  type <- match.arg(type, c("individualise", "regimen"))
  if (!inherits(model, "nm_model") || !is.data.frame(data) || !is.list(arguments)) {
    .lator_stop("Invalid LibeRator worker payload.")
  }
  if (type == "individualise") {
    return(do.call(LibeRation::nm_individual_fit, c(list(model = model, data = data), arguments)))
  }
  if (!"CANDIDATE" %in% names(data)) .lator_stop("Regimen worker data requires `CANDIDATE`.")
  endpoint <- lator_endpoint_validate(arguments$endpoint)
  eta <- arguments$eta %||% NULL
  residual <- isTRUE(arguments$residual)
  interval <- arguments$interval %||% NULL
  arguments[c("endpoint", "eta", "residual", "interval")] <- NULL
  pieces <- split(data, data$CANDIDATE)
  results <- lapply(names(pieces), function(id) {
    candidate_data <- pieces[[id]]
    candidate_data$CANDIDATE <- NULL
    eta_value <- if (is.list(eta) && !is.matrix(eta)) eta[[id]] else eta
    predicted <- do.call(LibeRation::nm_simulate, c(
      list(model = model, data = candidate_data, eta = eta_value, residual = residual), arguments
    ))
    if (!"SIM" %in% names(predicted)) predicted$SIM <- predicted$ID
    evaluation <- lator_endpoint_evaluate(endpoint, predicted[predicted$EVID == 0L, , drop = FALSE], interval = interval)
    list(candidate_id = id, predictions = predicted, evaluation = evaluation)
  })
  names(results) <- names(pieces)
  list(schema = "liberator.regimen-worker-result", version = 1L, results = results)
}
