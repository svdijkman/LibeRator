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
#' backup, monitoring, validation, and clinical governance. The current
#' Research label denotes validation status, not a separate product edition.
#'
#' @param workspace Optional unlocked workspace.
#' @param path Workspace path when `workspace` is not supplied.
#' @param passphrase Optional passphrase. If omitted, an unlock screen is shown.
#' @param key Optional managed 32-byte key.
#' @param models Session models in addition to encrypted registrations.
#' @param endpoints Session endpoints in addition to encrypted registrations.
#' @param library_root Optional LibeRary catalogue root used by automatic
#'   qualified-model selection.
#' @param session_workspace Create a separate ephemeral encrypted workspace for
#'   every browser session. This is intended for hosted research demonstrations
#'   and prevents application users from sharing a workspace directory.
#' @param teaching_example Seed an otherwise empty workspace with the synthetic
#'   AED teaching patient, model, individual efficacy and safety endpoints, and
#'   their combined dual-endpoint objective. Intended for demonstrations; all
#'   targets are explicitly non-clinical.
#' @param host,port,launch.browser Passed to [shiny::runApp()].
#' @param allow_remote Explicitly permit a non-loopback bind for governed test deployments.
#' @return Invisibly, the Shiny app.
#' @export
lator_gui <- function(workspace = NULL, path = NULL, passphrase = NULL, key = NULL,
                      models = NULL, endpoints = NULL, library_root = NULL,
                      host = "127.0.0.1",
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
  teaching <- if (isTRUE(teaching_example)) {
    lator_example_dual_endpoint()
  } else NULL
  if (!is.null(teaching)) {
    if (!"teaching-aed" %in% names(supplied_models)) {
      supplied_models[["teaching-aed"]] <- teaching$model
    }
    teaching_endpoint_id <- teaching$endpoint$id
    teaching_endpoints <- teaching$endpoints %||%
      stats::setNames(list(teaching$endpoint), teaching_endpoint_id)
    for (endpoint_id in names(teaching_endpoints)) {
      if (!endpoint_id %in% names(supplied_endpoints)) {
        supplied_endpoints[[endpoint_id]] <- teaching_endpoints[[endpoint_id]]
      }
    }
  }
  favicon <- system.file("assets", "favicon.svg", package = "LibeRator")
  if (!nzchar(favicon)) favicon <- file.path(getwd(), "LibeRator", "inst", "assets", "favicon.svg")
  prefix <- paste0("liberator-assets-", substr(.lator_id("gui"), 5, 16))
  if (file.exists(favicon)) shiny::addResourcePath(prefix, dirname(favicon))
  favicon_href <- if (file.exists(favicon)) paste0(prefix, "/favicon.svg") else ""
  hosted <- nzchar(Sys.getenv("LIBER_HOSTED")) ||
    nzchar(Sys.getenv("RSCONNECT_USER")) ||
    nzchar(Sys.getenv("SHINYAPPS_ACCOUNT"))
  liberary_catalog_available <- requireNamespace("LibeRary", quietly = TRUE) &&
    all(c(
      "library_get",
      "library_list",
      "library_model"
    ) %in% getNamespaceExports("LibeRary"))
  liberary_available <- liberary_catalog_available &&
    all(c(
      "library_clinical_qualifications",
      "library_get",
      "library_model"
    ) %in% getNamespaceExports("LibeRary"))
  model_discovery <- liberary_available &&
    (!hosted || (!is.null(library_root) && dir.exists(path.expand(library_root))))
  model_discovery_reason <- if (!liberary_available) {
    "The installed LibeRary package does not provide the current clinical-qualification API."
  } else if (hosted && !model_discovery) {
    "Automatic LibeRary matching is unavailable in this hosted deployment because no governed catalogue was mounted."
  } else ""
  model_library_available <- liberary_catalog_available &&
    (!hosted || (!is.null(library_root) && dir.exists(path.expand(library_root))))
  model_library_reason <- if (!liberary_catalog_available) {
    "Install the current LibeRary package to browse medication-specific models."
  } else if (hosted && !model_library_available) {
    "The LibeRary catalogue is not mounted in this hosted deployment."
  } else ""

  ui <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$title("LibeRator"),
      if (nzchar(favicon_href)) htmltools::tags$link(rel = "icon", type = "image/svg+xml", href = favicon_href),
      htmltools::tags$script(htmltools::HTML(
        "(function(){try{var t=localStorage.getItem('liber.theme');if(t!=='dark'&&t!=='light'){var l=localStorage.getItem('liberatorTheme');t=l==='dark'?'dark':l==='light'?'light':(matchMedia('(prefers-color-scheme: dark)').matches?'dark':'light');}document.documentElement.setAttribute('data-liber-theme',t);}catch(e){}})();"
      )),
      htmltools::tags$style(htmltools::HTML(
        "html,body{margin:0;min-height:100%;background:#f1f6f6;font-family:'Segoe UI',Arial,sans-serif}html[data-liber-theme='dark'] body{background:#152426}.lator-unlock{min-height:100vh;display:grid;place-items:center;background:radial-gradient(circle at 20% 10%,#dceeee,transparent 35%),#f4f8f8}.lator-unlock-card{width:min(430px,calc(100vw - 40px));background:#fff;border:1px solid #cbdede;border-radius:18px;padding:32px;box-shadow:0 18px 55px rgba(25,74,76,.13)}.lator-unlock-brand{display:flex;align-items:center;gap:13px;margin-bottom:4px}.lator-unlock-brand img{width:52px;height:52px}.lator-unlock-card h1{color:#145c60;margin:0}.lator-unlock-card p{color:#617779;line-height:1.5}.lator-unlock-version{display:inline-block;margin-bottom:10px;color:#617779;font-size:12px}.lator-unlock-card .form-control{border-radius:9px;border-color:#bdd4d4}.lator-unlock-card .btn{width:100%;background:#19787b;color:#fff;border:0;border-radius:9px;margin-top:12px}.lator-safety{font-size:12px;border-left:3px solid #39999a;padding-left:10px;margin-top:20px}"
      ))
    ),
    htmltools::tags$body(shiny::uiOutput("lator_app", container = htmltools::tags$div))
  )

  server <- function(input, output, session) {
    tasks <- .liber_shared_task_registry(session)
    task_signal <- shiny::reactiveVal(0L)
    session_path <- if (isTRUE(session_workspace)) {
      base <- path %||% file.path(tempdir(), "LibeRator-cloud")
      file.path(base, "sessions", gsub("[^A-Za-z0-9_-]", "-", session$token))
    } else path
    state <- shiny::reactiveValues(
      workspace = initial_workspace, patient_id = NULL, models = supplied_models,
      endpoints = supplied_endpoints, patient_endpoints = list(),
      model_id = NULL, endpoint_id = NULL,
      drug_id = NULL, endpoint_prompt = 0L,
      regimen = NULL, selected_candidate = NULL, prediction = NULL,
      model_selection = NULL,
      model_library = list(), model_library_loaded = FALSE,
      data_revision = 0L,
      status = list(level = "info", text = "Workbench ready")
    )
    invalidate_workspace_data <- function() {
      state$data_revision <- as.integer(state$data_revision %||% 0L) + 1L
      invisible(state$data_revision)
    }
    available_endpoints <- function() {
      patient_keys <- names(state$patient_endpoints %||% list())
      c(
        state$patient_endpoints %||% list(),
        state$endpoints[setdiff(names(state$endpoints), patient_keys)]
      )
    }
    restore_therapy <- function(patient_id, preferred_drug = NULL,
                                prompt_if_missing = FALSE) {
      state$drug_id <- NULL
      state$endpoint_id <- NULL
      state$patient_endpoints <- list()
      state$model_id <- NULL
      state$model_selection <- NULL
      state$model_library <- list()
      state$model_library_loaded <- FALSE
      if (is.null(patient_id) || !length(patient_id) ||
          is.na(patient_id[[1L]]) || !nzchar(patient_id[[1L]])) {
        return(invisible(NULL))
      }
      patient <- lator_patient_get(state$workspace, patient_id[[1L]])
      medications <- lator_patient_medications(patient)
      if (!nrow(medications)) return(invisible(NULL))
      preferred_key <- if (!is.null(preferred_drug) && length(preferred_drug) &&
                            !is.na(preferred_drug[[1L]]) &&
                            nzchar(preferred_drug[[1L]])) {
        candidate <- as.character(preferred_drug[[1L]])
        if (candidate %in% medications$key) candidate else .lator_drug_key(candidate)
      } else ""
      index <- match(preferred_key, medications$key)
      if (is.na(index)) index <- 1L
      medication <- medications[index, , drop = FALSE]
      state$drug_id <- medication$key[[1L]]
      endpoint_key <- medication$endpoint_key[[1L]]
      profile <- lator_patient_endpoint_get(
        patient, medication$drug[[1L]]
      )
      history <- profile$endpoint_history %||% list()
      for (selection in history) {
        history_key <- as.character(selection$endpoint_key %||% "")
        history_endpoint <- selection$endpoint_snapshot %||% NULL
        if (nzchar(history_key) && !is.null(history_endpoint)) {
          history_endpoint <- tryCatch(
            lator_endpoint_validate(history_endpoint),
            error = function(error) NULL
          )
          if (!is.null(history_endpoint)) {
            state$patient_endpoints[[history_key]] <- history_endpoint
          }
        }
      }
      endpoint_snapshot <- profile$endpoint_snapshot %||% NULL
      if (!is.null(endpoint_snapshot)) {
        endpoint_snapshot <- lator_endpoint_validate(endpoint_snapshot)
        state$patient_endpoints[[endpoint_key]] <- endpoint_snapshot
        state$endpoint_id <- endpoint_key
      } else if (nzchar(endpoint_key) &&
                 endpoint_key %in% names(state$endpoints)) {
        state$endpoint_id <- endpoint_key
      } else if (isTRUE(prompt_if_missing)) {
        state$endpoint_prompt <- as.integer(state$endpoint_prompt %||% 0L) + 1L
      }
      state$regimen <- NULL
      state$selected_candidate <- NULL
      state$prediction <- NULL
      invisible(medication)
    }
    persist_endpoint_preference <- function(endpoint_key = NULL,
                                            endpoint = NULL,
                                            actor = "endpoint-selection") {
      shiny::req(state$patient_id, state$drug_id)
      endpoints <- available_endpoints()
      if (is.null(endpoint)) endpoint <- endpoints[[endpoint_key]]
      if (is.null(endpoint)) .lator_stop("Unknown endpoint: ", endpoint_key)
      endpoint <- lator_endpoint_validate(endpoint)
      patient <- lator_patient_get(state$workspace, state$patient_id)
      medications <- lator_patient_medications(patient)
      medication <- medications[
        medications$key == state$drug_id, , drop = FALSE
      ]
      if (nrow(medication) != 1L) {
        .lator_stop("The selected medication is no longer on the patient timeline.")
      }
      existing <- lator_patient_endpoint_get(
        patient, medication$drug[[1L]]
      )
      instance_key <- .lator_patient_endpoint_instance_key(
        patient, medication$drug[[1L]], endpoint
      )
      if (!is.null(existing) &&
          !is.null(existing$endpoint_snapshot) &&
          identical(existing$endpoint_hash, .lator_hash(endpoint))) {
        state$patient_endpoints[[existing$endpoint_key]] <- endpoint
        return(existing$endpoint_key)
      }
      patient <- lator_patient_endpoint_set(
        patient, medication$drug[[1L]], instance_key, endpoint,
        therapeutic_class = medication$therapeutic_class[[1L]]
      )
      saved <- lator_patient_save(
        state$workspace, patient,
        expected_revision = patient$revision,
        actor = actor
      )
      state$patient_endpoints[[instance_key]] <- endpoint
      invalidate_workspace_data()
      instance_key
    }
    persist_model_preference <- function(
        model_id, actor = "model-selection") {
      shiny::req(state$patient_id, state$drug_id)
      model_id <- as.character(model_id %||% "")
      model_id <- if (length(model_id) && !is.na(model_id[[1L]])) {
        model_id[[1L]]
      } else ""
      if (nzchar(model_id) && !model_id %in% names(state$models)) {
        .lator_stop("Unknown population model: ", model_id)
      }
      patient <- lator_patient_get(state$workspace, state$patient_id)
      medications <- lator_patient_medications(patient)
      medication <- medications[
        medications$key == state$drug_id, , drop = FALSE
      ]
      if (nrow(medication) != 1L) {
        .lator_stop(
          "The selected medication is no longer on the patient timeline."
        )
      }
      model_hash <- if (nzchar(model_id)) {
        .lator_hash(state$models[[model_id]])
      } else ""
      existing <- .lator_patient_model_get(
        patient, medication$drug[[1L]]
      )
      if (!is.null(existing) &&
          identical(existing$model_id, model_id) &&
          identical(existing$model_hash, model_hash)) {
        return(model_id)
      }
      patient <- .lator_patient_model_set(
        patient, medication$drug[[1L]], model_id, model_hash
      )
      lator_patient_save(
        state$workspace, patient,
        expected_revision = patient$revision, actor = actor
      )
      invalidate_workspace_data()
      model_id
    }
    restore_model_selection <- function(patient_id) {
      state$model_selection <- NULL
      if (is.null(patient_id) || !length(patient_id) || is.na(patient_id[[1L]]) ||
          !nzchar(patient_id[[1L]])) return(invisible(NULL))
      patient_id <- as.character(patient_id[[1L]])
      patient <- lator_patient_get(state$workspace, patient_id)
      medications <- lator_patient_medications(patient)
      medication <- medications[
        medications$key == state$drug_id, , drop = FALSE
      ]
      preference <- if (nrow(medication) == 1L) {
        .lator_patient_model_get(patient, medication$drug[[1L]])
      } else NULL
      if (!is.null(preference) && nzchar(preference$model_id)) {
        if (preference$model_id %in% names(state$models)) {
          current_hash <- .lator_hash(state$models[[preference$model_id]])
          if (!nzchar(preference$model_hash) ||
              identical(preference$model_hash, current_hash)) {
            state$model_id <- preference$model_id
            return(invisible(preference))
          }
          state$status <- list(
            level = "warning",
            text = paste(
              "The previously selected population model has changed;",
              "review and select it again"
            )
          )
          return(invisible(preference))
        }
      }
      selections <- patient$model_selections %||% list()
      if (!is.null(state$drug_id) && length(selections)) {
        selections <- Filter(function(selection) {
          drug <- as.character(selection$endpoint$drug %||% "")
          nzchar(drug) && identical(.lator_drug_key(drug), state$drug_id)
        }, selections)
      }
      if (!length(selections)) return(invisible(NULL))
      selection <- utils::tail(selections, 1L)[[1L]]
      state$model_selection <- selection
      if (!identical(selection$status, "selected") ||
          !nzchar(selection$selected_model_id %||% "")) {
        return(invisible(selection))
      }
      key <- .lator_selection_model_key(selection)
      if (key %in% names(state$models)) {
        state$model_id <- key
        return(invisible(selection))
      }
      imported <- tryCatch({
        arguments <- list(
          library_id = selection$selected_model_id,
          qualification_id = selection$selected_qualification_id
        )
        if (!is.null(library_root)) arguments$root <- library_root
        do.call(lator_model_from_liberary, arguments)
      }, error = function(error) NULL)
      if (!is.null(imported)) {
        if (!is.null(state$drug_id)) {
          medications <- lator_patient_medications(patient)
          medication <- medications[
            medications$key == state$drug_id, , drop = FALSE
          ]
          if (nrow(medication) == 1L) {
            attr(imported, "lator_medications") <-
              medication$drug[[1L]]
          }
        }
        state$models[[key]] <- imported
        state$model_id <- key
      }
      invisible(selection)
    }
    hydrate <- function(workspace) {
      registered_models <- .lator_registered_models(workspace)
      registered_endpoints <- .lator_registered_endpoints(workspace)
      hydrated_models <- c(registered_models, supplied_models[setdiff(names(supplied_models), names(registered_models))])
      hydrated_endpoints <- c(registered_endpoints, supplied_endpoints[setdiff(names(supplied_endpoints), names(registered_endpoints))])
      patients <- lator_patient_list(workspace)
      if (!is.null(teaching) && !nrow(patients)) {
        teaching_patient <- lator_patient_endpoint_set(
          teaching$patient, teaching$endpoint$drug,
          teaching_endpoint_id, teaching$endpoint,
          therapeutic_class = "antiseizure"
        )
        teaching_patient <- lator_patient_save(workspace, teaching_patient,
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
      state$patient_id <- if (nrow(patients)) {
        patients$patient_id[[1L]]
      } else NULL
      restore_therapy(state$patient_id, prompt_if_missing = FALSE)
      restore_model_selection(state$patient_id)
    }
    # A pre-unlocked workspace is hydrated while Shiny is constructing the
    # server function, before any observer/render consumer is active. Isolate
    # startup reads from `state` so reactiveValues are never accessed without a
    # reactive context.
    if (!is.null(initial_workspace)) {
      shiny::isolate(hydrate(initial_workspace))
    }

    output$lator_app <- shiny::renderUI({
      if (is.null(state$workspace)) return(htmltools::tags$div(class = "lator-unlock",
        htmltools::tags$div(class = "lator-unlock-card",
          htmltools::tags$div(class = "lator-unlock-brand",
                             htmltools::tags$img(src = favicon_href, alt = ""),
                             htmltools::tags$h1("LibeRator")),
          htmltools::tags$span(class = "lator-unlock-version",
                              paste0("Version ", utils::packageVersion("LibeRator"))),
          htmltools::tags$p("Unlock or create the encrypted LibeRator workspace."),
          shiny::passwordInput("lator_passphrase", "Workspace passphrase", placeholder = "At least 12 characters"),
          shiny::actionButton("lator_unlock", "Unlock workspace"),
          htmltools::tags$p(
            class = "lator-safety",
            "Research status: LibeRator is being developed for eventual clinical use but is not yet clinically validated. Outputs require qualified human review."
          )
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
        state$workspace, state$patient_id, state$models,
        available_endpoints(),
        state$model_id, state$endpoint_id, state$regimen,
        state$selected_candidate, state$prediction, state$status,
        icon = favicon_href,
        task = .liber_shared_task_snapshot(tasks),
        model_selection = state$model_selection,
        selected_drug = state$drug_id,
        endpoint_prompt = state$endpoint_prompt,
        hosted = hosted,
        model_discovery = model_discovery,
        model_discovery_reason = model_discovery_reason,
        model_library = state$model_library,
        model_library_loaded = state$model_library_loaded,
        model_library_available = model_library_available,
        model_library_reason = model_library_reason
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
          restore_therapy(
            state$patient_id, prompt_if_missing = TRUE
          )
          restore_model_selection(state$patient_id)
        } else if (action == "delete_patient") {
          shiny::req(state$patient_id)
          deleted_id <- state$patient_id
          lator_patient_delete(
            state$workspace, deleted_id,
            confirmation = as.character(event$confirmation %||% ""),
            actor = "patient-deletion"
          )
          patients <- lator_patient_list(state$workspace)
          state$patient_id <- if (nrow(patients)) {
            patients$patient_id[[1L]]
          } else NULL
          restore_therapy(state$patient_id, prompt_if_missing = FALSE)
          restore_model_selection(state$patient_id)
          invalidate_workspace_data()
          state$status <- list(
            level = "success",
            text = paste("Deleted patient", deleted_id)
          )
        } else if (action == "select_drug") {
          shiny::req(state$patient_id)
          restore_therapy(
            state$patient_id,
            preferred_drug = as.character(event$id %||% ""),
            prompt_if_missing = TRUE
          )
          restore_model_selection(state$patient_id)
        } else if (action == "select_model") {
          state$model_id <- persist_model_preference(
            as.character(event$id %||% ""),
            actor = "manual-model-selection"
          )
          state$regimen <- NULL
          state$selected_candidate <- NULL; state$prediction <- NULL
        } else if (action == "load_model_library") {
          shiny::req(state$patient_id, state$drug_id)
          if (!isTRUE(model_library_available)) {
            .lator_stop(model_library_reason)
          }
          patient <- lator_patient_get(state$workspace, state$patient_id)
          medications <- lator_patient_medications(patient)
          medication <- medications[
            medications$key == state$drug_id, , drop = FALSE
          ]
          if (nrow(medication) != 1L) {
            .lator_stop("The selected medication is no longer on the patient timeline.")
          }
          arguments <- list(drug = medication$drug[[1L]])
          if (!is.null(library_root)) arguments$root <- library_root
          state$model_library <- do.call(
            .lator_liberary_models_for_drug, arguments
          )
          state$model_library_loaded <- TRUE
        } else if (action == "model_import_library") {
          shiny::req(state$patient_id, state$drug_id)
          if (!isTRUE(model_library_available)) {
            .lator_stop(model_library_reason)
          }
          patient <- lator_patient_get(state$workspace, state$patient_id)
          medications <- lator_patient_medications(patient)
          medication <- medications[
            medications$key == state$drug_id, , drop = FALSE
          ]
          if (nrow(medication) != 1L) {
            .lator_stop("The selected medication is no longer on the patient timeline.")
          }
          arguments <- list(drug = medication$drug[[1L]])
          if (!is.null(library_root)) arguments$root <- library_root
          scoped_models <- do.call(.lator_liberary_models_for_drug, arguments)
          library_id <- .lator_scalar(event$library_id, "library_id")
          selected <- Filter(function(item) {
            identical(item$id, library_id)
          }, scoped_models)
          if (length(selected) != 1L) {
            .lator_stop(
              "The requested LibeRary model is not scoped to the selected medication."
            )
          }
          selected <- selected[[1L]]
          if (isTRUE(selected$researchAcknowledgementRequired) &&
              !isTRUE(event$acknowledge_research)) {
            .lator_stop(
              "Acknowledge the Research status before importing this review-stage model."
            )
          }
          import_arguments <- list(
            library_id = library_id,
            allow_unvalidated = isTRUE(
              selected$researchAcknowledgementRequired
            )
          )
          if (!is.null(library_root)) import_arguments$root <- library_root
          model <- do.call(lator_model_from_liberary, import_arguments)
          attr(model, "lator_medications") <- medication$drug[[1L]]
          model_key <- paste0("liberary-", library_id)
          provenance <- attr(
            model, "library_provenance", exact = TRUE
          ) %||% list()
          provenance$selection_mode <- "manual"
          provenance$selected_medication <- medication$drug[[1L]]
          lator_model_register(
            state$workspace, model, id = model_key,
            name = attr(model, "name", exact = TRUE) %||% selected$title,
            qualification = list(
              status = if (isTRUE(selected$clinicallyQualified)) {
                "qualified"
              } else "research",
              catalogue_status = selected$status,
              clinical_status = selected$clinicalStatus
            ),
            endpoint_ids = state$endpoint_id %||% character(),
            provenance = provenance,
            actor = "manual-model-selection"
          )
          state$models[[model_key]] <- model
          state$model_id <- persist_model_preference(
            model_key, actor = "manual-model-selection"
          )
          state$model_selection <- NULL
          state$regimen <- NULL
          state$selected_candidate <- NULL
          state$prediction <- NULL
          state$status <- list(
            level = if (isTRUE(selected$clinicallyQualified)) {
              "success"
            } else "warning",
            text = paste("Selected LibeRary model", selected$title)
          )
        } else if (action == "model_create_template") {
          shiny::req(state$patient_id, state$drug_id)
          patient <- lator_patient_get(state$workspace, state$patient_id)
          medications <- lator_patient_medications(patient)
          medication <- medications[
            medications$key == state$drug_id, , drop = FALSE
          ]
          if (nrow(medication) != 1L) {
            .lator_stop("The selected medication is no longer on the patient timeline.")
          }
          model <- .lator_model_from_template_event(event)
          attr(model, "lator_medications") <- medication$drug[[1L]]
          model_key <- .lator_id("manual-model")
          template_id <- as.character(
            event$template_id %||% paste0("ADVAN", event$advan %||% "")
          )
          lator_model_register(
            state$workspace, model, id = model_key,
            name = attr(model, "name", exact = TRUE) %||% model_key,
            qualification = list(
              status = "research",
              source = "manual-liberation-template"
            ),
            endpoint_ids = state$endpoint_id %||% character(),
            provenance = list(
              source = "LibeRation",
              creation = "template",
              template = template_id,
              selected_medication = medication$drug[[1L]],
              created_at = .lator_now()
            ),
            actor = "manual-model-template"
          )
          state$models[[model_key]] <- model
          state$model_id <- persist_model_preference(
            model_key, actor = "manual-model-template"
          )
          state$model_selection <- NULL
          state$regimen <- NULL
          state$selected_candidate <- NULL
          state$prediction <- NULL
          state$status <- list(
            level = "success",
            text = paste(
              "Created and selected",
              attr(model, "name", exact = TRUE) %||% "model",
              "from a LibeRation template"
            )
          )
        } else if (action == "select_endpoint") {
          endpoint_key <- as.character(event$id %||% "")
          if (!nzchar(endpoint_key)) {
            state$endpoint_id <- NULL
            state$model_selection <- NULL
            return()
          }
          state$endpoint_id <- persist_endpoint_preference(endpoint_key)
          state$regimen <- NULL
          state$selected_candidate <- NULL; state$prediction <- NULL
          state$model_selection <- NULL
        }
        else if (action == "new_patient") {
          patient <- lator_patient_new(event$patient_id, event$study_id %||% "", event$label %||% "")
          patient <- lator_patient_save(state$workspace, patient)
          state$patient_id <- patient$patient_id
          restore_therapy(state$patient_id, prompt_if_missing = FALSE)
          invalidate_workspace_data()
          state$status <- list(level = "success", text = paste("Created", patient$patient_id))
        } else if (action == "add_medication") {
          shiny::req(state$patient_id)
          drug <- trimws(as.character(event$drug %||% ""))
          therapeutic_class <- trimws(as.character(
            event$therapeutic_class %||% ""
          ))
          if (!nzchar(therapeutic_class)) {
            preset <- .lator_endpoint_preset_for_drug(drug)
            therapeutic_class <- as.character(
              preset$therapeutic_class %||% ""
            )
          }
          patient <- lator_patient_get(state$workspace, state$patient_id)
          patient <- lator_patient_medication_add(
            patient, drug, therapeutic_class = therapeutic_class,
            monitoring_analytes = trimws(strsplit(
              as.character(event$monitoring_analytes %||% ""),
              "[;\r\n]+", perl = TRUE
            )[[1L]])
          )
          patient <- lator_patient_save(
            state$workspace, patient,
            expected_revision = patient$revision,
            actor = "medication-addition"
          )
          invalidate_workspace_data()
          restore_therapy(
            state$patient_id, preferred_drug = drug,
            prompt_if_missing = isTRUE(event$configure_endpoint)
          )
          restore_model_selection(state$patient_id)
          state$status <- list(
            level = "success",
            text = paste(
              "Added", drug, "to the patient treatment profile",
              if (isTRUE(event$configure_endpoint)) "- choose an endpoint next"
              else ""
            )
          )
        } else if (action == "add_event") {
          shiny::req(state$patient_id)
          patient <- lator_patient_get(state$workspace, state$patient_id)
          if (event$type %in% c("dose", "concentration")) {
            medications <- lator_patient_medications(patient)
            treatment_drug <- trimws(as.character(
              event$treatment_drug %||% event$name %||% ""
            ))
            medication_key <- if (nzchar(treatment_drug)) {
              .lator_drug_key(treatment_drug)
            } else ""
            medication <- medications[
              medications$key == medication_key, , drop = FALSE
            ]
            if (nrow(medication) != 1L) {
              .lator_stop(
                "Dose and TDM evidence must reference a medication already ",
                "added to this patient treatment profile."
              )
            }
            allowed_analytes <- unique(c(
              medication$drug[[1L]],
              strsplit(
                medication$monitoring_analytes[[1L]] %||% "",
                "|", fixed = TRUE
              )[[1L]]
            ))
            allowed_analytes <- allowed_analytes[nzchar(allowed_analytes)]
            if (identical(event$type, "dose")) {
              event$name <- medication$drug[[1L]]
            } else if (!tolower(trimws(as.character(event$name))) %in%
                       tolower(allowed_analytes)) {
              .lator_stop(
                "The selected TDM analyte is not registered for ",
                medication$drug[[1L]], "."
              )
            }
          }
          value <- if (is.null(event$value) || !nzchar(as.character(event$value))) NA_real_ else as.numeric(event$value)
          metadata <- list()
          if (event$type == "dose") {
            dosing_interval <- suppressWarnings(as.numeric(
              event$dosing_interval %||% ""
            ))
            if (!length(dosing_interval) || is.na(dosing_interval)) {
              dosing_interval <- 0
            }
            if (!is.finite(dosing_interval) || dosing_interval < 0) {
              .lator_stop("Dosing interval must be a non-negative number of hours.")
            }
            steady_state <- isTRUE(event$steady_state)
            if (steady_state && dosing_interval <= 0) {
              .lator_stop(
                "A positive dosing interval is required for a steady-state dose."
              )
            }
            metadata <- list(
              route = event$route %||% "oral",
              cmt = as.integer(event$cmt %||% 1L),
              rate = as.numeric(event$rate %||% 0),
              ii = dosing_interval,
              ss = as.integer(steady_state),
              drug = medication$drug[[1L]],
              therapeutic_class = medication$therapeutic_class[[1L]]
            )
          }
          if (event$type == "concentration") metadata <- list(
            drug = medication$drug[[1L]],
            analyte = as.character(event$name)
          )
          patient <- lator_patient_add_event(
            patient, event$type, as.numeric(event$time), event$name %||% "", value,
            event$unit %||% "", missing_reason = event$missing_reason %||% "", metadata = metadata
          )
          patient <- lator_patient_save(state$workspace, patient)
          invalidate_workspace_data()
          if (identical(event$type, "dose")) {
            restore_therapy(
              state$patient_id, preferred_drug = event$name,
              prompt_if_missing = TRUE
            )
            restore_model_selection(state$patient_id)
          }
          state$status <- list(
            level = "success",
            text = paste("Evidence added to the immutable timeline - revision", patient$revision)
          )
        } else if (action == "correct_event") {
          shiny::req(state$patient_id)
          patient <- lator_patient_get(state$workspace, state$patient_id)
          event_id <- .lator_scalar(
            event$event_id %||% "", "event_id", max_chars = 128L
          )
          reason <- .lator_scalar(
            event$reason %||% "", "reason", max_chars = 1000L
          )
          actor <- .lator_scalar(
            event$actor %||% "local-clinician", "actor", max_chars = 128L
          )
          entered_in_error <- isTRUE(event$entered_in_error)
          active_events <- .lator_active_events(patient)
          active_ids <- vapply(
            active_events, `[[`, character(1), "event_id"
          )
          location <- match(event_id, active_ids)
          if (is.na(location)) {
            .lator_stop(
              "This evidence is no longer active. Refresh the ledger and ",
              "amend its current replacement instead."
            )
          }
          original <- active_events[[location]]
          replacement <- list()
          corrected_type <- original$type
          if (!entered_in_error) {
            corrected_name <- as.character(
              event$name %||% original$name
            )
            metadata <- list()
            if (corrected_type %in% c("dose", "concentration")) {
              medications <- lator_patient_medications(patient)
              treatment_drug <- trimws(as.character(
                event$treatment_drug %||%
                  original$metadata$drug %||% corrected_name
              ))
              medication <- medications[
                medications$key == .lator_drug_key(treatment_drug),
                , drop = FALSE
              ]
              if (nrow(medication) != 1L) {
                .lator_stop(
                  "Corrected dose and TDM evidence must reference a ",
                  "medication already added to this patient."
                )
              }
              allowed_analytes <- unique(c(
                medication$drug[[1L]],
                strsplit(
                  medication$monitoring_analytes[[1L]] %||% "",
                  "|", fixed = TRUE
                )[[1L]]
              ))
              allowed_analytes <- allowed_analytes[nzchar(allowed_analytes)]
              if (identical(corrected_type, "dose")) {
                corrected_name <- medication$drug[[1L]]
                dosing_interval <- suppressWarnings(as.numeric(
                  event$dosing_interval %||% original$metadata$ii %||% 0
                ))
                if (!is.finite(dosing_interval) || dosing_interval < 0) {
                  .lator_stop(
                    "Dosing interval must be a non-negative number of hours."
                  )
                }
                steady_state <- isTRUE(event$steady_state)
                if (steady_state && dosing_interval <= 0) {
                  .lator_stop(
                    "A positive dosing interval is required for a ",
                    "steady-state dose."
                  )
                }
                metadata <- list(
                  route = event$route %||%
                    original$metadata$route %||% "oral",
                  cmt = as.integer(
                    event$cmt %||% original$metadata$cmt %||% 1L
                  ),
                  rate = as.numeric(
                    event$rate %||% original$metadata$rate %||% 0
                  ),
                  ii = dosing_interval,
                  ss = as.integer(steady_state),
                  drug = medication$drug[[1L]],
                  therapeutic_class =
                    medication$therapeutic_class[[1L]]
                )
              } else {
                if (!tolower(trimws(corrected_name)) %in%
                    tolower(allowed_analytes)) {
                  .lator_stop(
                    "The corrected TDM analyte is not registered for ",
                    medication$drug[[1L]], "."
                  )
                }
                metadata <- list(
                  drug = medication$drug[[1L]],
                  analyte = corrected_name
                )
              }
            }
            corrected_value <- if (
              is.null(event$value) ||
                !nzchar(trimws(as.character(event$value)))
            ) {
              NA_real_
            } else suppressWarnings(as.numeric(event$value))
            replacement <- list(
              time = as.numeric(event$time),
              name = corrected_name,
              value = corrected_value,
              unit = as.character(event$unit %||% original$unit),
              source = as.character(event$source %||% original$source),
              missing_reason = as.character(
                event$missing_reason %||% original$missing_reason
              ),
              metadata = metadata
            )
          }
          patient <- lator_patient_correct_event(
            patient, event_id = event_id, reason = reason,
            replacement = replacement,
            entered_in_error = entered_in_error, actor = actor
          )
          correction_event <- Filter(function(item) {
            identical(
              as.character(item$supersedes %||% ""), event_id
            )
          }, patient$events)[[1L]]
          patient <- lator_patient_save(
            state$workspace, patient, actor = actor
          )
          .lator_audit_append(
            state$workspace, "evidence_corrected", "patient",
            patient$patient_id,
            detail = list(
              patient_revision = patient$revision,
              original_event_id = event_id,
              correction_event_id = correction_event$event_id,
              correction_action = if (entered_in_error) {
                "entered_in_error"
              } else "replacement",
              reason = reason
            ),
            actor = actor
          )
          invalidate_workspace_data()
          state$regimen <- NULL
          state$selected_candidate <- NULL
          state$prediction <- NULL
          if (identical(corrected_type, "dose")) {
            restore_therapy(
              state$patient_id,
              preferred_drug = correction_event$metadata$drug %||%
                original$metadata$drug %||% original$name,
              prompt_if_missing = FALSE
            )
            restore_model_selection(state$patient_id)
          }
          state$status <- list(
            level = "success",
            text = paste(
              if (entered_in_error) {
                "Evidence marked as entered in error"
              } else "Evidence corrected",
              "- original retained - revision", patient$revision
            )
          )
        } else if (action %in% c(
          "create_endpoint_set", "revise_endpoint_set"
        )) {
          shiny::req(state$patient_id, state$drug_id)
          revising <- identical(action, "revise_endpoint_set")
          original_key <- as.character(event$original_key %||% "")
          available <- available_endpoints()
          original <- if (revising && nzchar(original_key)) {
            available[[original_key]]
          } else NULL
          if (revising &&
              (is.null(original) ||
               !identical(original$kind, "multi_endpoint") ||
               !identical(original_key, state$endpoint_id))) {
            .lator_stop(
              "The multi-endpoint objective being modified is no longer selected."
            )
          }
          patient <- lator_patient_get(state$workspace, state$patient_id)
          medications <- lator_patient_medications(patient)
          medication <- medications[
            medications$key == state$drug_id, , drop = FALSE
          ]
          if (nrow(medication) != 1L) {
            .lator_stop(
              "The selected medication is no longer on the patient timeline."
            )
          }
          component_input <- event$components %||% list()
          if (!is.list(component_input) || length(component_input) < 2L) {
            .lator_stop("Select at least two endpoint components.")
          }
          components <- lapply(component_input, function(item) {
            key <- as.character(item$endpoint_key %||% "")
            definition <- available[[key]]
            if (is.null(definition) ||
                identical(definition$kind, "multi_endpoint") ||
                !identical(
                  .lator_drug_key(definition$drug),
                  medication$key[[1L]]
                )) {
              .lator_stop(
                "Every objective component must be an available endpoint ",
                "for the selected medication."
              )
            }
            lator_endpoint_component(
              definition,
              role = as.character(item$role %||% "secondary"),
              weight = as.numeric(item$weight %||% 1),
              hard_constraint = isTRUE(item$hard_constraint),
              minimum_attainment = as.numeric(
                item$minimum_attainment %||% 0.9
              )
            )
          })
          name <- .lator_scalar(
            event$name, "name", max_chars = 256L
          )
          version <- .lator_scalar(
            event$version %||% "1.0.0", "version", max_chars = 32L
          )
          source <- .lator_scalar(
            event$source, "source", max_chars = 1000L
          )
          objective_id <- if (revising) original$id else paste0(
            "objective-", medication$key[[1L]], "-",
            gsub(
              "(^-+|-+$)", "",
              gsub("[^a-z0-9]+", "-", tolower(name))
            )
          )
          endpoint <- lator_endpoint_set(
            id = objective_id, name = name,
            drug = medication$drug[[1L]], components = components,
            source = source,
            status = as.character(event$status %||% "draft"),
            version = version,
            metadata = if (revising) list(
              supersedes_endpoint_key = original_key,
              patient_specific_revision = TRUE
            ) else list(patient_specific_objective = TRUE)
          )
          current_profile <- lator_patient_endpoint_get(
            patient, medication$drug[[1L]]
          )
          history <- current_profile$endpoint_history %||% list()
          identity_exists <- any(vapply(history, function(item) {
            identical(item$endpoint_id, endpoint$id) &&
              identical(item$endpoint_version, endpoint$version)
          }, logical(1)))
          if (identity_exists) {
            .lator_stop(
              "Endpoint objective ", endpoint$id, "@", endpoint$version,
              " already exists for this patient and medication. ",
              "Use a new objective version."
            )
          }
          state$endpoint_id <- persist_endpoint_preference(
            endpoint = endpoint,
            actor = if (revising) {
              "multi-endpoint-modification"
            } else "multi-endpoint-creation"
          )
          state$model_selection <- NULL
          state$regimen <- NULL
          state$selected_candidate <- NULL
          state$prediction <- NULL
          state$status <- list(
            level = "success",
            text = paste(
              if (revising) {
                "Saved and selected revised multi-endpoint objective"
              } else "Created and selected multi-endpoint objective",
              endpoint$name, paste0("v", endpoint$version)
            )
          )
        } else if (action %in% c("create_endpoint", "revise_endpoint")) {
          shiny::req(state$patient_id, state$drug_id)
          revising <- identical(action, "revise_endpoint")
          original_key <- as.character(event$original_key %||% "")
          original <- if (revising && nzchar(original_key)) {
            available_endpoints()[[original_key]]
          } else NULL
          if (revising && (is.null(original) ||
                           !identical(original_key, state$endpoint_id))) {
            .lator_stop(
              "The endpoint being modified is no longer the selected endpoint."
            )
          }
          endpoint <- .lator_endpoint_from_template(
            as.character(event$template_id %||% ""),
            event$values %||% list()
          )
          if (revising) {
            if (!identical(endpoint$id, original$id) ||
                !identical(
                  .lator_drug_key(endpoint$drug),
                  .lator_drug_key(original$drug)
                )) {
              .lator_stop(
                "Endpoint modification cannot change its medication or identity."
              )
            }
            endpoint$metadata$supersedes_endpoint_key <- original_key
            endpoint$metadata$patient_specific_revision <- TRUE
          }
          patient <- lator_patient_get(state$workspace, state$patient_id)
          current_profile <- lator_patient_endpoint_get(
            patient, endpoint$drug
          )
          history <- current_profile$endpoint_history %||% list()
          identity_exists <- any(vapply(history, function(item) {
            identical(item$endpoint_id, endpoint$id) &&
              identical(item$endpoint_version, endpoint$version)
          }, logical(1)))
          if (identity_exists) {
            .lator_stop(
              "Endpoint ", endpoint$id, "@", endpoint$version,
              " already exists for this patient and medication. Use a new endpoint version."
            )
          }
          medication <- lator_patient_medications(patient)
          medication <- medication[
            medication$key == state$drug_id, , drop = FALSE
          ]
          if (nrow(medication) != 1L ||
              !identical(
                .lator_drug_key(endpoint$drug),
                medication$key[[1L]]
              )) {
            .lator_stop(
              "The configured endpoint must match the selected medication."
            )
          }
          state$endpoint_id <- persist_endpoint_preference(
            endpoint = endpoint,
            actor = if (revising) "endpoint-modification" else
              "endpoint-library"
          )
          state$model_selection <- NULL
          state$regimen <- NULL
          state$selected_candidate <- NULL
          state$prediction <- NULL
          state$status <- list(
            level = "success",
            text = paste(
              if (revising) "Saved and selected revised endpoint" else
                "Created and selected endpoint",
              endpoint$name, paste0("v", endpoint$version)
            )
          )
        } else if (action == "auto_select_model") {
          shiny::req(state$patient_id, state$endpoint_id)
          if (!isTRUE(model_discovery)) {
            .lator_stop(model_discovery_reason)
          }
          patient <- lator_patient_get(state$workspace, state$patient_id)
          arguments <- list(
            patient = patient,
            endpoint = available_endpoints()[[state$endpoint_id]]
          )
          if (!is.null(library_root)) arguments$root <- library_root
          selection <- do.call(lator_model_select_from_liberary, arguments)
          lator_model_selection_save(
            state$workspace, patient, selection, actor = "model-selection"
          )
          state$model_selection <- selection
          invalidate_workspace_data()
          if (identical(selection$status, "selected")) {
            import_arguments <- list(
              library_id = selection$selected_model_id,
              qualification_id = selection$selected_qualification_id
            )
            if (!is.null(library_root)) import_arguments$root <- library_root
            selected_model <- do.call(lator_model_from_liberary, import_arguments)
            medications <- lator_patient_medications(patient)
            medication <- medications[
              medications$key == state$drug_id, , drop = FALSE
            ]
            if (nrow(medication) == 1L) {
              attr(selected_model, "lator_medications") <-
                medication$drug[[1L]]
            }
            model_key <- .lator_selection_model_key(selection)
            selected_candidate <- Filter(function(candidate) {
              identical(candidate$id, selection$selected_model_id) &&
                identical(
                  candidate$qualification_id,
                  selection$selected_qualification_id
                )
            }, selection$candidates)
            selected_qualification <- if (length(selected_candidate)) {
              selected_candidate[[1L]]$qualification
            } else list(status = "qualified")
            lator_model_register(
              state$workspace, selected_model, id = model_key,
              name = attr(selected_model, "name", exact = TRUE) %||%
                selection$selected_model_id,
              qualification = selected_qualification,
              endpoint_ids = state$endpoint_id,
              provenance = attr(
                selected_model, "library_provenance", exact = TRUE
              ) %||% list(),
              actor = "model-selection"
            )
            state$models[[model_key]] <- selected_model
            state$model_id <- persist_model_preference(
              model_key, actor = "model-selection"
            )
            state$status <- list(
              level = "success",
              text = paste("Selected qualified model", selection$selected_model_id)
            )
          } else {
            state$model_id <- NULL
            state$status <- list(
              level = "warning",
              text = if (identical(selection$status, "multiple_suitable_models")) {
                "Several qualified models are similarly suitable; automatic selection was withheld"
              } else {
                "No suitable qualified LibeRary model is available for this patient context"
              }
            )
          }
        } else if (action == "assess") {
          shiny::req(state$patient_id, state$model_id, state$endpoint_id)
          patient <- lator_patient_get(state$workspace, state$patient_id)
          endpoint <- available_endpoints()[[state$endpoint_id]]
          assessment_mode <- as.character(event$mode %||% "static")
          if (identical(assessment_mode, "dynamic")) {
            dynamic_status <- .lator_dynamic_evidence_status(
              patient, endpoint$drug
            )
            if (!isTRUE(dynamic_status$ready)) {
              .lator_stop(dynamic_status$reason)
            }
          }
          observation_scope <- as.character(
            event$profile_observation_scope %||% "automatic"
          )
          observation_count <- suppressWarnings(as.integer(
            event$profile_observation_count %||% 2L
          ))
          observation_since <- suppressWarnings(as.numeric(
            event$profile_observation_since %||% NA_real_
          ))
          .liber_shared_task_start(
            tasks, "LibeRator", ".lator_gui_background_task",
            args = list(
              operation = "assess",
              arguments = list(
                patient = patient,
                model = state$models[[state$model_id]],
                endpoint = endpoint,
                mode = assessment_mode,
                process_scale = as.numeric(event$process_scale %||% 0.1),
                profile_observation_scope = observation_scope,
                profile_observation_count = observation_count,
                profile_observation_since = observation_since
              )
            ),
            label = paste(assessment_mode, "patient assessment"),
            metadata = list(
              operation = "assess",
              patient_id = patient$patient_id,
              patient_revision = patient$revision,
              drug_id = state$drug_id,
              model_id = state$model_id,
              endpoint_id = state$endpoint_id
            )
          )
          task_signal(task_signal() + 1L)
          .liber_shared_task_notify(
            session, "liberator_workbench", tasks
          )
        } else if (action == "optimise") {
          shiny::req(state$patient_id, state$drug_id)
          patient <- lator_patient_get(state$workspace, state$patient_id)
          assessments <- Filter(function(assessment) {
            drug <- as.character(
              assessment$analyte %||% assessment$endpoint$drug %||% ""
            )
            nzchar(drug) &&
              identical(.lator_drug_key(drug), state$drug_id)
          }, patient$assessments)
          if (!length(assessments)) {
            .lator_stop("Run an individual assessment for this medication first.")
          }
          assessment <- utils::tail(assessments, 1L)[[1L]]
          parse_numbers <- function(value) as.numeric(strsplit(gsub("[[:space:]]", "", value), ",", fixed = TRUE)[[1L]])
          candidates <- lator_regimen_candidates(
            parse_numbers(event$amounts), parse_numbers(event$intervals),
            horizon = as.numeric(event$horizon %||% 168)
          )
          .liber_shared_task_start(
            tasks, "LibeRator", ".lator_gui_background_task",
            args = list(
              operation = "optimise",
              arguments = list(
                assessment = assessment,
                patient = patient,
                candidates = candidates,
                nsim = as.integer(event$nsim %||% 100L),
                grid_step = as.numeric(event$grid_step %||% 0.5)
              )
            ),
            label = "Candidate regimen comparison",
            metadata = list(
              operation = "optimise",
              patient_id = patient$patient_id,
              patient_revision = patient$revision,
              drug_id = state$drug_id,
              assessment_id = assessment$assessment_id
            )
          )
          task_signal(task_signal() + 1L)
          state$selected_candidate <- NULL
          state$prediction <- NULL
          .liber_shared_task_notify(
            session, "liberator_workbench", tasks
          )
        } else if (action == "select_regimen") {
          shiny::req(state$regimen)
          candidate_id <- as.character(event$id %||% "")
          if (!candidate_id %in% state$regimen$summary$candidate_id) .lator_stop("Unknown regimen candidate.")
          selected <- as.character(
            state$selected_candidate %||% character()
          )
          if (candidate_id %in% selected) {
            selected <- setdiff(selected, candidate_id)
          } else {
            selected <- c(selected, candidate_id)
          }
          state$selected_candidate <- selected
          predictions <- state$prediction %||% list()
          if (length(predictions) && !is.null(names(predictions))) {
            state$prediction <- predictions[
              intersect(selected, names(predictions))
            ]
          } else {
            state$prediction <- NULL
          }
          state$status <- list(
            level = "info",
            text = if (length(selected)) {
              paste(length(selected), "regimen(s) selected for prediction")
            } else "No regimens selected"
          )
        } else if (action == "predict_regimen") {
          shiny::req(state$regimen, state$selected_candidate)
          .liber_shared_task_start(
            tasks, "LibeRator", ".lator_gui_background_task",
            args = list(
              operation = "predict",
              arguments = list(
                regimen = state$regimen,
                candidate_id = as.character(state$selected_candidate)
              )
            ),
            label = "Selected-regimen future prediction",
            metadata = list(
              operation = "predict",
              patient_id = state$patient_id,
              drug_id = state$drug_id,
              candidate_id = as.character(state$selected_candidate),
              assessment_id = state$regimen$assessment_id
            )
          )
          task_signal(task_signal() + 1L)
          .liber_shared_task_notify(
            session, "liberator_workbench", tasks
          )
        } else if (action == "cancel_task") {
          if (.liber_shared_task_cancel_all(tasks)) {
            state$status <- list(
              level = "warning", text = "Background calculation cancelled"
            )
            .liber_shared_task_notify(
              session, "liberator_workbench", tasks
            )
          }
        }
      }, error = function(error) {
        state$status <- list(level = "error", text = conditionMessage(error))
        shiny::showNotification(conditionMessage(error), type = "error", duration = 9)
      })
    }, ignoreInit = TRUE)

    shiny::observe({
      task_signal()
      if (!.liber_shared_task_active(tasks)) return()
      shiny::invalidateLater(100, session)
      .liber_shared_task_poll(tasks)
      completed <- .liber_shared_task_take_completed(tasks)
      if (!length(completed)) return()
      for (job in completed) {
        if (identical(job$status, "failed")) {
          state$status <- list(level = "error", text = job$error)
          shiny::showNotification(job$error, type = "error", duration = 9)
          next
        }
        if (!identical(job$status, "completed")) next
        operation <- job$metadata$operation
        if (!identical(job$metadata$patient_id, state$patient_id)) {
          state$status <- list(
            level = "warning",
            text = "The active patient changed; the stale result was discarded"
          )
          next
        }
        if (nzchar(as.character(job$metadata$drug_id %||% "")) &&
            !identical(job$metadata$drug_id, state$drug_id)) {
          state$status <- list(
            level = "warning",
            text = "The active medication changed; the stale result was discarded"
          )
          next
        }
        if (identical(operation, "assess")) {
          patient <- lator_patient_get(state$workspace, state$patient_id)
          if (!identical(
            as.integer(patient$revision),
            as.integer(job$metadata$patient_revision)
          )) {
            state$status <- list(
              level = "warning",
              text = paste(
                "New patient evidence was recorded while the assessment ran;",
                "the stale posterior was discarded"
              )
            )
            next
          }
          assessment <- job$result
          assessment$patient_revision <- patient$revision + 1L
          patient$assessments <- c(patient$assessments, list(assessment))
          lator_patient_save(
            state$workspace, patient,
            expected_revision = patient$revision,
            actor = "local-session"
          )
          state$regimen <- NULL
          state$selected_candidate <- NULL
          state$prediction <- NULL
          invalidate_workspace_data()
          state$status <- list(
            level = "success",
            text = paste(
              "Assessment completed in",
              round(
                assessment$diagnostics$elapsed_total_seconds, 2
              ), "s"
            )
          )
        } else if (identical(operation, "optimise")) {
          patient <- lator_patient_get(state$workspace, state$patient_id)
          latest <- if (length(patient$assessments)) {
            utils::tail(patient$assessments, 1L)[[1L]]$assessment_id
          } else ""
          if (!identical(
            as.integer(patient$revision),
            as.integer(job$metadata$patient_revision)
          ) || !identical(latest, job$metadata$assessment_id)) {
            state$status <- list(
              level = "warning",
              text = "The patient assessment changed; the stale regimen result was discarded"
            )
            next
          }
          state$regimen <- job$result
          state$selected_candidate <- NULL
          state$prediction <- NULL
          state$status <- list(
            level = "success",
            text = paste(
              "Regimen comparison completed;",
              "select a regimen to forecast"
            )
          )
        } else if (identical(operation, "predict")) {
          if (is.null(state$regimen) ||
              !setequal(
                as.character(state$selected_candidate),
                as.character(job$metadata$candidate_id)
              ) ||
              !identical(
                state$regimen$assessment_id,
                job$metadata$assessment_id
              )) {
            state$status <- list(
              level = "warning",
              text = "The selected regimen changed; the stale prediction was discarded"
            )
            next
          }
          state$prediction <- job$result
          state$status <- list(
            level = "success",
            text = paste(
              length(state$prediction),
              "future regimen prediction(s) ready"
            )
          )
        }
      }
      .liber_shared_task_notify(session, "liberator_workbench", tasks)
    })
  }
  app <- shiny::shinyApp(ui, server)
  if (is.null(launch.browser)) return(app)
  shiny::runApp(app, host = host, port = port, launch.browser = launch.browser)
  invisible(app)
}
