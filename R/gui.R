.lator_named_models <- function(models) {
  if (is.null(models)) return(list())
  if (inherits(models, "nm_model")) models <- list(model = models)
  if (!is.list(models) || any(!vapply(models, inherits, logical(1), "nm_model"))) {
    .lator_stop("`models` must be an nm_model or named list of nm_model objects.")
  }
  if (is.null(names(models)) || any(!nzchar(names(models)))) names(models) <- paste0("model-", seq_along(models))
  models
}

.lator_named_endpoints <- function(endpoints) {
  if (is.null(endpoints)) return(list())
  if (inherits(endpoints, "lator_endpoint")) endpoints <- list(endpoint = endpoints)
  if (!is.list(endpoints)) .lator_stop("`endpoints` must contain LibeRator endpoints.")
  endpoints <- lapply(endpoints, lator_endpoint_validate)
  if (is.null(names(endpoints)) || any(!nzchar(names(endpoints)))) names(endpoints) <- vapply(endpoints, `[[`, character(1), "id")
  endpoints
}

#' Launch the LibeRator longitudinal dosing workbench
#'
#' By default the server binds only to loopback. Exposing it to another host is
#' deliberately blocked unless `allow_remote = TRUE`; production deployment
#' additionally requires institutional authentication, TLS, authorization,
#' backup, monitoring, validation, and clinical governance outside this
#' research package.
#'
#' @param workspace Optional unlocked workspace.
#' @param path Workspace path when `workspace` is not supplied.
#' @param passphrase Optional passphrase. If omitted, an unlock screen is shown.
#' @param key Optional managed 32-byte key.
#' @param models Session models in addition to encrypted registrations.
#' @param endpoints Session endpoints in addition to encrypted registrations.
#' @param session_workspace Create a separate ephemeral encrypted workspace for
#'   every browser session. This is intended for hosted research demonstrations
#'   and prevents application users from sharing a workspace directory.
#' @param teaching_example Seed an otherwise empty workspace with the synthetic
#'   AED teaching patient, model, and endpoint. Intended for demonstrations;
#'   the example is explicitly non-clinical.
#' @param host,port,launch.browser Passed to [shiny::runApp()].
#' @param allow_remote Explicitly permit a non-loopback bind for governed test deployments.
#' @return Invisibly, the Shiny app.
#' @export
lator_gui <- function(workspace = NULL, path = NULL, passphrase = NULL, key = NULL,
                      models = NULL, endpoints = NULL, host = "127.0.0.1",
                      port = NULL, launch.browser = TRUE, allow_remote = FALSE,
                      session_workspace = FALSE, teaching_example = FALSE) {
  if (!host %in% c("127.0.0.1", "localhost", "::1") && !isTRUE(allow_remote)) {
    .lator_stop("Non-loopback hosting is disabled. Set `allow_remote = TRUE` only behind governed authentication and TLS.")
  }
  if (isTRUE(session_workspace) && !is.null(workspace)) {
    .lator_stop("`workspace` cannot be supplied when `session_workspace = TRUE`.")
  }
  initial_workspace <- workspace
  if (!is.null(initial_workspace)) initial_workspace <- .lator_require_workspace(initial_workspace)
  if (is.null(initial_workspace) && (!is.null(passphrase) || !is.null(key))) {
    if (isTRUE(session_workspace)) {
      .lator_stop("`passphrase` and `key` cannot pre-unlock a session-isolated workspace.")
    }
    initial_workspace <- lator_workspace(path, passphrase, key, create = TRUE)
  }
  supplied_models <- .lator_named_models(models)
  supplied_endpoints <- .lator_named_endpoints(endpoints)
  teaching <- if (isTRUE(teaching_example)) lator_example_aed() else NULL
  if (!is.null(teaching)) {
    if (!"teaching-aed" %in% names(supplied_models)) {
      supplied_models[["teaching-aed"]] <- teaching$model
    }
    teaching_endpoint_id <- teaching$endpoint$id
    if (!teaching_endpoint_id %in% names(supplied_endpoints)) {
      supplied_endpoints[[teaching_endpoint_id]] <- teaching$endpoint
    }
  }
  favicon <- system.file("assets", "favicon.svg", package = "LibeRator")
  if (!nzchar(favicon)) favicon <- file.path(getwd(), "LibeRator", "inst", "assets", "favicon.svg")
  prefix <- paste0("liberator-assets-", substr(.lator_id("gui"), 5, 16))
  if (file.exists(favicon)) shiny::addResourcePath(prefix, dirname(favicon))
  favicon_href <- if (file.exists(favicon)) paste0(prefix, "/favicon.svg") else ""

  ui <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$title("LibeRator"),
      if (nzchar(favicon_href)) htmltools::tags$link(rel = "icon", type = "image/svg+xml", href = favicon_href),
      htmltools::tags$style(htmltools::HTML(
        "html,body{margin:0;min-height:100%;background:#f1f6f6;font-family:Inter,Segoe UI,sans-serif}.lator-unlock{min-height:100vh;display:grid;place-items:center;background:radial-gradient(circle at 20% 10%,#dceeee,transparent 35%),#f4f8f8}.lator-unlock-card{width:min(430px,calc(100vw - 40px));background:#fff;border:1px solid #cbdede;border-radius:18px;padding:32px;box-shadow:0 18px 55px rgba(25,74,76,.13)}.lator-unlock-card h1{color:#145c60;margin:0}.lator-unlock-card p{color:#617779;line-height:1.5}.lator-unlock-card .form-control{border-radius:9px;border-color:#bdd4d4}.lator-unlock-card .btn{width:100%;background:#19787b;color:#fff;border:0;border-radius:9px;margin-top:12px}.lator-safety{font-size:12px;border-left:3px solid #39999a;padding-left:10px;margin-top:20px}"
      ))
    ),
    htmltools::tags$body(shiny::uiOutput("lator_app", container = htmltools::tags$div))
  )

  server <- function(input, output, session) {
    session_path <- if (isTRUE(session_workspace)) {
      base <- path %||% file.path(tempdir(), "LibeRator-cloud")
      file.path(base, "sessions", gsub("[^A-Za-z0-9_-]", "-", session$token))
    } else path
    state <- shiny::reactiveValues(
      workspace = initial_workspace, patient_id = NULL, models = supplied_models,
      endpoints = supplied_endpoints, model_id = NULL, endpoint_id = NULL,
      regimen = NULL, selected_candidate = NULL, prediction = NULL,
      data_revision = 0L,
      status = list(level = "info", text = "Workbench ready")
    )
    invalidate_workspace_data <- function() {
      state$data_revision <- as.integer(state$data_revision %||% 0L) + 1L
      invisible(state$data_revision)
    }
    hydrate <- function(workspace) {
      registered_models <- .lator_registered_models(workspace)
      registered_endpoints <- .lator_registered_endpoints(workspace)
      hydrated_models <- c(registered_models, supplied_models[setdiff(names(supplied_models), names(registered_models))])
      hydrated_endpoints <- c(registered_endpoints, supplied_endpoints[setdiff(names(supplied_endpoints), names(registered_endpoints))])
      patients <- lator_patient_list(workspace)
      if (!is.null(teaching) && !nrow(patients)) {
        teaching_patient <- lator_patient_save(workspace, teaching$patient,
                                               actor = "synthetic-teaching-seed")
        patients <- lator_patient_list(workspace)
        invalidate_workspace_data()
        state$status <- list(
          level = "info",
          text = paste("Loaded synthetic teaching patient", teaching_patient$patient_id)
        )
      }
      state$models <- hydrated_models
      state$endpoints <- hydrated_endpoints
      state$model_id <- names(hydrated_models)[1L] %||% NULL
      state$endpoint_id <- names(hydrated_endpoints)[1L] %||% NULL
      state$patient_id <- patients$patient_id[1L] %||% NULL
    }
    if (!is.null(initial_workspace)) hydrate(initial_workspace)

    output$lator_app <- shiny::renderUI({
      if (is.null(state$workspace)) return(htmltools::tags$div(class = "lator-unlock",
        htmltools::tags$div(class = "lator-unlock-card",
          htmltools::tags$h1("LibeRator"), htmltools::tags$p("Unlock or create the encrypted research workspace."),
          shiny::passwordInput("lator_passphrase", "Workspace passphrase", placeholder = "At least 12 characters"),
          shiny::actionButton("lator_unlock", "Unlock workspace"),
          htmltools::tags$p(class = "lator-safety", "Research and teaching use only. Outputs require qualified human review and are not autonomous treatment instructions.")
        )
      ))
      liberatorWorkbenchOutput("lator_workbench")
    })

    shiny::observeEvent(input$lator_unlock, {
      tryCatch({
        unlocked_workspace <- lator_workspace(session_path, input$lator_passphrase,
                                              create = TRUE)
        state$workspace <- unlocked_workspace
        hydrate(unlocked_workspace)
      }, error = function(error) shiny::showNotification(conditionMessage(error), type = "error", duration = 8))
    })

    workbench_payload <- shiny::reactive({
      shiny::req(state$workspace)
      state$data_revision
      .lator_gui_payload(
        state$workspace, state$patient_id, state$models, state$endpoints,
        state$model_id, state$endpoint_id, state$regimen,
        state$selected_candidate, state$prediction, state$status,
        icon = favicon_href
      )
    })

    output$lator_workbench <- renderLiberatorWorkbench({
      liberator_workbench(workbench_payload())
    })

    shiny::observeEvent(input$liberator_workbench_event, {
      event <- input$liberator_workbench_event; action <- as.character(event$action %||% "")
      tryCatch({
        if (action == "select_patient") {
          state$patient_id <- as.character(event$id); state$regimen <- NULL
          state$selected_candidate <- NULL; state$prediction <- NULL
        } else if (action == "select_model") {
          state$model_id <- as.character(event$id); state$regimen <- NULL
          state$selected_candidate <- NULL; state$prediction <- NULL
        } else if (action == "select_endpoint") {
          state$endpoint_id <- as.character(event$id); state$regimen <- NULL
          state$selected_candidate <- NULL; state$prediction <- NULL
        }
        else if (action == "new_patient") {
          patient <- lator_patient_new(event$patient_id, event$study_id %||% "", event$label %||% "")
          patient <- lator_patient_save(state$workspace, patient)
          state$patient_id <- patient$patient_id
          invalidate_workspace_data()
          state$status <- list(level = "success", text = paste("Created", patient$patient_id))
        } else if (action == "add_event") {
          shiny::req(state$patient_id)
          patient <- lator_patient_get(state$workspace, state$patient_id)
          value <- if (is.null(event$value) || !nzchar(as.character(event$value))) NA_real_ else as.numeric(event$value)
          metadata <- list()
          if (event$type == "dose") metadata <- list(
            route = event$route %||% "oral", cmt = as.integer(event$cmt %||% 1L),
            rate = as.numeric(event$rate %||% 0)
          )
          patient <- lator_patient_add_event(
            patient, event$type, as.numeric(event$time), event$name %||% "", value,
            event$unit %||% "", missing_reason = event$missing_reason %||% "", metadata = metadata
          )
          patient <- lator_patient_save(state$workspace, patient)
          invalidate_workspace_data()
          state$status <- list(
            level = "success",
            text = paste("Evidence added to the immutable timeline - revision", patient$revision)
          )
        } else if (action == "assess") {
          shiny::req(state$patient_id, state$model_id, state$endpoint_id)
          patient <- lator_patient_get(state$workspace, state$patient_id)
          state$status <- list(level = "working", text = "Updating the individual posterior...")
          assessment <- lator_assess(
            patient, state$models[[state$model_id]], state$endpoints[[state$endpoint_id]],
            mode = event$mode %||% "static", process_scale = as.numeric(event$process_scale %||% 0.1),
            workspace = state$workspace
          )
          invalidate_workspace_data()
          state$status <- list(level = "success", text = paste("Assessment completed in", round(assessment$diagnostics$elapsed_total_seconds, 2), "s"))
        } else if (action == "optimise") {
          shiny::req(state$patient_id)
          patient <- lator_patient_get(state$workspace, state$patient_id)
          if (!length(patient$assessments)) .lator_stop("Run an individual assessment first.")
          assessment <- utils::tail(patient$assessments, 1L)[[1L]]
          parse_numbers <- function(value) as.numeric(strsplit(gsub("[[:space:]]", "", value), ",", fixed = TRUE)[[1L]])
          candidates <- lator_regimen_candidates(
            parse_numbers(event$amounts), parse_numbers(event$intervals),
            horizon = as.numeric(event$horizon %||% 168)
          )
          state$status <- list(level = "working", text = "Comparing candidate regimens...")
          state$regimen <- lator_regimen_optimise(
            assessment, patient, candidates, nsim = as.integer(event$nsim %||% 100L),
            grid_step = as.numeric(event$grid_step %||% 0.5)
          )
          state$selected_candidate <- NULL
          state$prediction <- NULL
          state$status <- list(level = "success", text = "Regimen comparison completed; select a regimen to forecast")
        } else if (action == "select_regimen") {
          shiny::req(state$regimen)
          candidate_id <- as.character(event$id %||% "")
          if (!candidate_id %in% state$regimen$summary$candidate_id) .lator_stop("Unknown regimen candidate.")
          state$selected_candidate <- candidate_id
          state$prediction <- NULL
          state$status <- list(level = "info", text = paste("Selected", candidate_id, "for future prediction"))
        } else if (action == "predict_regimen") {
          shiny::req(state$regimen, state$selected_candidate)
          state$prediction <- lator_regimen_predict(state$regimen, state$selected_candidate)
          state$status <- list(level = "success", text = paste("Future prediction ready for", state$selected_candidate))
        }
      }, error = function(error) {
        state$status <- list(level = "error", text = conditionMessage(error))
        shiny::showNotification(conditionMessage(error), type = "error", duration = 9)
      })
    }, ignoreInit = TRUE)
  }
  app <- shiny::shinyApp(ui, server)
  if (is.null(launch.browser)) return(app)
  shiny::runApp(app, host = host, port = port, launch.browser = launch.browser)
  invisible(app)
}
