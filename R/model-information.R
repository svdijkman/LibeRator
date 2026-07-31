.lator_control_parameter_labels <- function(lines, block = "THETA") {
  lines <- as.character(lines %||% character())
  header <- which(grepl(
    paste0("^\\s*\\$", toupper(block), "\\b"),
    toupper(lines), perl = TRUE
  ))
  if (!length(header)) return(character())
  start <- header[[1L]] + 1L
  following <- which(
    seq_along(lines) >= start & grepl("^\\s*\\$", lines)
  )
  end <- if (length(following)) following[[1L]] - 1L else length(lines)
  if (start > end) return(character())
  definitions <- lines[start:end]
  definitions <- definitions[
    nzchar(trimws(definitions)) & !grepl("^\\s*;", definitions)
  ]
  unname(vapply(definitions, function(line) {
    pieces <- strsplit(line, ";", fixed = TRUE)[[1L]]
    if (length(pieces) > 1L) trimws(paste(pieces[-1L], collapse = ";")) else ""
  }, character(1)))
}

.lator_model_assignments <- function(model) {
  code <- paste(
    as.character(model$PRED %||% ""),
    as.character(model$DES %||% ""),
    sep = "\n"
  )
  statements <- trimws(unlist(strsplit(code, "[;\\r\\n]+", perl = TRUE)))
  matched <- regexec(
    "^([A-Za-z][A-Za-z0-9_.]*)\\s*=\\s*(.+)$",
    statements, perl = TRUE
  )
  pieces <- regmatches(statements, matched)
  pieces <- Filter(function(item) length(item) == 3L, pieces)
  if (!length(pieces)) {
    return(data.frame(
      name = character(), expression = character(),
      stringsAsFactors = FALSE
    ))
  }
  output <- data.frame(
    name = vapply(pieces, `[[`, character(1), 2L),
    expression = vapply(pieces, `[[`, character(1), 3L),
    stringsAsFactors = FALSE
  )
  output <- output[!duplicated(output$name), , drop = FALSE]
  rownames(output) <- NULL
  output
}

.lator_model_output_kind <- function(model, name) {
  name <- toupper(as.character(name %||% ""))
  if (!nzchar(name)) return("derived")
  if (grepl("^(S|A)[0-9]+$", name) ||
      name %in% c("F", "Y", "PRED", "IPRED")) {
    return("internal")
  }
  assignments <- .lator_model_assignments(model)
  expression <- assignments$expression[
    match(name, toupper(assignments$name))
  ]
  common_parameter <- grepl(
    paste0(
      "^(CL|V|V[0-9]+|VC|VP[0-9]*|KA|K[0-9A-Z]+|Q[0-9]*|",
      "VM|VMAX|KM|EMAX|EC50|IC50|KIN|KOUT|BASE|TV[A-Z0-9_]+|",
      "F[0-9]+|D[0-9]+|ALAG[0-9]+)$"
    ),
    name
  )
  if (common_parameter ||
      (length(expression) && !is.na(expression) &&
       grepl("\\b(?:THETA|ETA)\\s*\\(", expression, perl = TRUE))) {
    "parameter"
  } else "derived"
}

.lator_model_eta_dependencies <- function(model, names) {
  assignments <- .lator_model_assignments(model)
  expressions <- stats::setNames(
    assignments$expression, toupper(assignments$name)
  )
  depends_on_eta <- function(name, seen = character()) {
    key <- toupper(as.character(name))
    if (!nzchar(key) || key %in% seen || !key %in% names(expressions)) {
      return(FALSE)
    }
    expression <- expressions[[key]]
    if (grepl(
      "\\bETA\\s*\\(", expression, perl = TRUE, ignore.case = TRUE
    )) return(TRUE)
    symbols <- tryCatch(
      unique(all.vars(parse(text = expression))),
      error = function(error) {
        unique(regmatches(
          expression,
          gregexpr("\\b[A-Za-z][A-Za-z0-9_.]*\\b", expression, perl = TRUE)
        )[[1L]])
      }
    )
    dependencies <- intersect(toupper(symbols), names(expressions))
    any(vapply(
      dependencies,
      depends_on_eta,
      logical(1),
      seen = c(seen, key)
    ))
  }
  vapply(names, depends_on_eta, logical(1))
}

.lator_model_output_details <- function(model, names) {
  names <- as.character(names %||% character())
  assignments <- .lator_model_assignments(model)
  expressions <- stats::setNames(assignments$expression, assignments$name)
  data.frame(
    name = names,
    kind = vapply(names, function(name) {
      .lator_model_output_kind(model, name)
    }, character(1)),
    expression = vapply(names, function(name) {
      expressions[[name]] %||% ""
    }, character(1)),
    individualised = .lator_model_eta_dependencies(model, names),
    stringsAsFactors = FALSE
  )
}

.lator_advan_description <- function(model) {
  descriptions <- c(
    `1` = "One-compartment intravenous bolus model",
    `2` = "One-compartment model with first-order extravascular absorption",
    `3` = "Two-compartment intravenous model",
    `4` = "Two-compartment model with first-order extravascular absorption",
    `5` = "General linear compartment model",
    `6` = "General nonlinear ordinary differential-equation model",
    `7` = "General linear compartment model",
    `8` = "General stiff differential-equation model",
    `9` = "General differential-algebraic/equilibrium model",
    `10` = "One-compartment model with Michaelis-Menten elimination",
    `11` = "Three-compartment intravenous model",
    `12` = "Three-compartment model with first-order extravascular absorption",
    `13` = "General ordinary differential-equation model",
    `14` = "General differential-equation model"
  )
  descriptions[[as.character(model$ADVAN)]] %||%
    paste0("ADVAN", model$ADVAN, " model")
}

.lator_known_covariate_description <- function(name) {
  descriptions <- c(
    WT = "Body weight",
    WEIGHT = "Body weight",
    AGE = "Age",
    SEX = "Sex indicator",
    COMED_VPA = "Concomitant valproate indicator",
    COMED_PHT = "Concomitant phenytoin indicator",
    COMED_PHB = "Concomitant phenobarbital indicator",
    COMED_CBZ = "Concomitant carbamazepine indicator",
    CRCL = "Creatinine clearance",
    EGFR = "Estimated glomerular filtration rate"
  )
  descriptions[[toupper(name)]] %||% "Model covariate"
}

.lator_known_derived_description <- function(name, expression) {
  descriptions <- c(
    NIND = paste(
      "Count of the phenytoin, phenobarbital and carbamazepine",
      "enzyme-inducer indicators."
    ),
    IND = paste(
      "Multiple-inducer indicator: 1 when more than one inducer is present;",
      "otherwise 0."
    )
  )
  descriptions[[toupper(name)]] %||%
    if (nzchar(expression)) paste0("Derived as: ", expression) else
      "Intermediate quantity calculated by the model."
}

.lator_assignment_targets <- function(assignments, token) {
  if (!nrow(assignments)) return(character())
  assignments$name[grepl(token, assignments$expression, perl = TRUE)]
}

.lator_model_limitations <- function(model, provenance) {
  limitations <- character()
  status <- tolower(as.character(
    provenance$status_at_import %||% provenance$status %||% ""
  ))
  metadata <- provenance$model_metadata %||% list()
  qualification <- provenance$clinical_qualification %||% NULL
  clinical <- provenance$qualification$clinical_use %||% list()
  records <- c(if (!is.null(qualification)) list(qualification) else list(), clinical)
  recorded <- unlist(lapply(records, function(record) {
    as.character(record$limitations %||% character())
  }), use.names = FALSE)
  limitations <- c(limitations, recorded[nzchar(recorded)])
  if (!identical(status, "validated")) {
    status_label <- if (nzchar(status)) status else "unrecorded"
    limitations <- c(
      limitations,
      paste0(
        "Catalogue status is '", status_label,
        "' rather than validated."
      )
    )
  }
  if (isTRUE(metadata$generated_suggestion) ||
      isTRUE(metadata$mapping_review_required)) {
    limitations <- c(
      limitations,
      "The executable model translation requires independent source verification."
    )
  }
  qualified <- any(vapply(records, function(record) {
    identical(record$status %||% "", "qualified")
  }, logical(1)))
  if (!qualified) {
    limitations <- c(
      limitations,
      "No current qualified clinical-use record is bound to this model."
    )
  }
  evidence <- unlist(lapply(records, function(record) {
    as.character(record$evidence$clinical_validation %||% "")
  }), use.names = FALSE)
  if (any(tolower(evidence) %in% c("not performed", "none", "not_performed"))) {
    limitations <- c(
      limitations,
      "Clinical validation is recorded as not performed."
    )
  }
  if (length(model$COVARIATES)) {
    limitations <- c(
      limitations,
      paste0(
        "Predictions depend on complete and correctly encoded covariates: ",
        paste(model$COVARIATES, collapse = ", "), "."
      )
    )
  }
  population <- trimws(as.character(provenance$study$population %||% ""))
  route <- trimws(as.character(provenance$study$route %||% ""))
  if (nzchar(population) || nzchar(route)) {
    limitations <- c(
      limitations,
      "Applicability outside the recorded study population and route has not been established."
    )
  }
  unique(limitations[nzchar(trimws(limitations))])
}

.lator_model_info_for_gui <- function(model, id = "") {
  provenance <- attr(model, "library_provenance", exact = TRUE) %||% list()
  assignments <- .lator_model_assignments(model)
  labels <- attr(model, "lator_parameter_labels", exact = TRUE) %||% list()
  theta_labels <- as.character(labels$theta %||% character())
  theta <- lapply(seq_len(nrow(model$THETAS)), function(index) {
    targets <- .lator_assignment_targets(
      assignments,
      paste0("\\bTHETA\\s*\\(\\s*", index, "\\s*\\)")
    )
    label <- if (length(theta_labels) >= index) theta_labels[[index]] else ""
    list(
      group = "THETA", name = paste0("THETA", index),
      value = as.numeric(model$THETAS$Value[[index]]),
      description = if (nzchar(label)) label else if (length(targets)) {
        paste("Fixed effect used in", paste(targets, collapse = ", "))
      } else "Fixed-effect parameter",
      expression = ""
    )
  })
  omega <- lapply(seq_len(model$n_eta), function(index) {
    targets <- .lator_assignment_targets(
      assignments,
      paste0("\\bETA\\s*\\(\\s*", index, "\\s*\\)")
    )
    diagonal <- model$OMEGAS$Value[
      model$OMEGAS$ROW == index & model$OMEGAS$COL == index
    ]
    list(
      group = "OMEGA", name = paste0("ETA", index, " variance"),
      value = if (length(diagonal)) as.numeric(diagonal[[1L]]) else NULL,
      description = if (length(targets)) {
        paste("Inter-individual variability entering", paste(targets, collapse = ", "))
      } else paste0("Variance of ETA", index),
      expression = ""
    )
  })
  error_code <- gsub("\\s+", "", toupper(model$ERROR %||% ""))
  sigma <- lapply(seq_len(nrow(model$SIGMAS)), function(index) {
    kind <- if (grepl(
      paste0("F\\*\\(1\\+ERR\\(", index, "\\)\\)"), error_code
    )) "Proportional residual-error variance" else if (grepl(
      paste0("F\\+ERR\\(", index, "\\)"), error_code
    )) "Additive residual-error variance" else
      "Residual-error variance"
    list(
      group = "SIGMA", name = paste0("SIGMA", index),
      value = as.numeric(model$SIGMAS$Value[[index]]),
      description = kind, expression = ""
    )
  })
  covariates <- lapply(model$COVARIATES, function(name) {
    targets <- assignments$name[vapply(assignments$expression, function(code) {
      grepl(paste0("\\b", name, "\\b"), code, perl = TRUE)
    }, logical(1))]
    list(
      name = name,
      description = .lator_known_covariate_description(name),
      usedIn = unique(targets)
    )
  })
  outputs <- .lator_model_output_details(model, assignments$name)
  derived <- lapply(which(outputs$kind == "derived"), function(index) {
    list(
      name = outputs$name[[index]],
      expression = outputs$expression[[index]],
      description = .lator_known_derived_description(
        outputs$name[[index]], outputs$expression[[index]]
      )
    )
  })
  clinical <- provenance$clinical_qualification %||% NULL
  if (is.null(clinical)) {
    clinical_records <- provenance$qualification$clinical_use %||% list()
    clinical <- if (length(clinical_records)) {
      utils::tail(clinical_records, 1L)[[1L]]
    } else list()
  }
  list(
    id = id,
    name = attr(model, "name", exact = TRUE) %||%
      provenance$title %||% id,
    source = provenance$source %||% "Local/LibeRation model",
    libraryId = provenance$library_id %||% "",
    libraryVersion = provenance$library_version %||% "",
    catalogueStatus = provenance$status_at_import %||% "local",
    qualificationStatus = clinical$status %||% "research",
    structure = paste0(
      .lator_advan_description(model), " (ADVAN", model$ADVAN,
      "/TRANS", model$TRANS, ")."
    ),
    population = provenance$study$population %||% "",
    route = provenance$study$route %||% "",
    confidence = provenance$confidence$overall %||% NULL,
    parameters = c(theta, omega, sigma),
    covariates = covariates,
    derived = derived,
    limitations = .lator_model_limitations(model, provenance)
  )
}
