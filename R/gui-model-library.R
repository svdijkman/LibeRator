.lator_canonical_drug <- function(value) {
  value <- tolower(trimws(as.character(value %||% "")))
  value <- gsub("\\([^)]*\\)", " ", value)
  value <- gsub("[^a-z0-9]+", " ", value)
  value <- gsub("\\s+", " ", trimws(value))
  aliases <- c(
    "valproic acid" = "valproate",
    "sodium valproate" = "valproate",
    "valproate sodium" = "valproate",
    "phenobarbitone" = "phenobarbital",
    "phenytoin sodium" = "phenytoin",
    "carbamazepine cr" = "carbamazepine",
    "carbamazepine xr" = "carbamazepine",
    "oxcarbazepine mhd" = "oxcarbazepine",
    "10 hydroxycarbazepine" = "oxcarbazepine",
    "monohydroxy derivative" = "oxcarbazepine"
  )
  if (value %in% names(aliases)) unname(aliases[[value]]) else value
}

.lator_compound_drug_terms <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return(character())
  terms <- trimws(strsplit(
    value, "(?i)\\s*(?:;|/|\\||\\+|,|\\band\\b)\\s*", perl = TRUE
  )[[1L]])
  unique(vapply(terms[nzchar(terms)], .lator_canonical_drug, character(1)))
}

.lator_compound_matches_drug <- function(compound, drug) {
  target <- .lator_canonical_drug(drug)
  nzchar(target) && target %in% .lator_compound_drug_terms(compound)
}

.lator_liberary_models_for_drug <- function(drug, root = NULL) {
  drug <- .lator_scalar(drug, "drug", max_chars = 128L)
  list_models <- .lator_liberary_api("library_list")
  arguments <- list()
  if (!is.null(root)) arguments$root <- root
  entries <- do.call(list_models, arguments)
  if (!nrow(entries)) return(list())
  keep <- vapply(
    entries$compound, .lator_compound_matches_drug, logical(1), drug = drug
  )
  usable_status <- !tolower(entries$status) %in%
    c("stub", "rejected", "deprecated", "withdrawn")
  entries <- entries[keep & usable_status & is.finite(entries$advan), , drop = FALSE]
  if (!nrow(entries)) return(list())
  entries <- entries[order(
    !entries$clinically_qualified,
    tolower(entries$status) != "validated",
    -entries$confidence_overall,
    entries$title
  ), , drop = FALSE]
  unname(lapply(seq_len(nrow(entries)), function(index) {
    row <- entries[index, , drop = FALSE]
    list(
      id = as.character(row$library_id[[1L]]),
      title = as.character(row$title[[1L]]),
      compound = as.character(row$compound[[1L]]),
      population = as.character(row$population[[1L]]),
      status = as.character(row$status[[1L]]),
      version = as.character(row$version[[1L]]),
      advan = as.integer(row$advan[[1L]]),
      trans = as.integer(row$trans[[1L]]),
      modelType = as.character(row$model_type[[1L]]),
      confidence = if (is.finite(row$confidence_overall[[1L]])) {
        as.numeric(row$confidence_overall[[1L]])
      } else NULL,
      clinicalStatus = as.character(row$clinical_status[[1L]]),
      clinicallyQualified = isTRUE(row$clinically_qualified[[1L]]),
      researchAcknowledgementRequired =
        !identical(tolower(as.character(row$status[[1L]])), "validated")
    )
  }))
}

.lator_model_templates_for_gui <- function() {
  catalogue <- LibeRation::nm_structural_templates()
  advanced <- lapply(seq_len(nrow(catalogue)), function(index) list(
    id = as.character(catalogue$template[[index]]),
    name = as.character(catalogue$model[[index]]),
    notes = as.character(catalogue$notes[[index]]),
    type = "structural"
  ))
  c(list(list(
    id = "standard_advan",
    name = "Standard ADVAN 1-14 model",
    notes = "The same editable ADVAN templates used by LibeRation.",
    type = "advan"
  )), advanced)
}

.lator_model_from_template_event <- function(event) {
  template_type <- match.arg(
    as.character(event$template_type %||% "advan"),
    c("advan", "structural")
  )
  name <- .lator_scalar(
    event$name %||% "Patient population model", "name", max_chars = 256L
  )
  if (identical(template_type, "advan")) {
    advan <- as.integer(.lator_number(event$advan %||% 2L, "advan"))
    if (!advan %in% 1:14) .lator_stop("`advan` must be from 1 through 14.")
    ode_advan <- c(6L, 8L, 9L, 13L, 14L)
    trans <- if (advan %in% ode_advan) NULL else {
      value <- as.integer(.lator_number(event$trans %||% 2L, "trans"))
      if (!value %in% 1:6) .lator_stop("`trans` must be from 1 through 6.")
      value
    }
    n_state <- if (advan %in% ode_advan) {
      value <- as.integer(.lator_number(event$n_state %||% 2L, "n_state"))
      if (value < 1L || value > 20L) {
        .lator_stop("ODE templates support between 1 and 20 states.")
      }
      value
    } else NULL
    return(LibeRation::nm_advan_template(
      advan = advan, trans = trans, n_state = n_state, problem = name
    ))
  }
  template <- .lator_scalar(event$template_id, "template_id")
  if (!template %in% LibeRation::nm_structural_templates()$template) {
    .lator_stop("Unknown LibeRation template: ", template)
  }
  model <- LibeRation::nm_model_template(
    template = template,
    iiv = isTRUE(event$iiv),
    residual = as.character(event$residual %||% "proportional"),
    n_transit = as.integer(event$n_transit %||% 3L)
  )
  attr(model, "name") <- name
  model
}
