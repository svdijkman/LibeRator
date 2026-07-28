.lator_gui_background_task <- function(operation, arguments) {
  operation <- match.arg(operation, c("assess", "optimise", "predict"))
  if (!is.list(arguments)) {
    .lator_stop("Background GUI task arguments must be a list.")
  }
  switch(
    operation,
    assess = lator_assess(
      arguments$patient,
      arguments$model,
      arguments$endpoint,
      mode = arguments$mode,
      process_scale = arguments$process_scale,
      workspace = NULL
    ),
    optimise = lator_regimen_optimise(
      arguments$assessment,
      arguments$patient,
      arguments$candidates,
      nsim = arguments$nsim,
      grid_step = arguments$grid_step
    ),
    predict = stats::setNames(lapply(
      as.character(arguments$candidate_id),
      function(candidate_id) {
        lator_regimen_predict(arguments$regimen, candidate_id)
      }
    ), as.character(arguments$candidate_id))
  )
}
