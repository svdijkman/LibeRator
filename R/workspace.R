.lator_default_workspace <- function() {
  if (.Platform$OS.type == "windows") {
    home <- Sys.getenv("USERPROFILE", unset = path.expand("~"))
    file.path(home, "Documents", "LibeR", "liberator-workspace")
  } else file.path(path.expand("~"), "LibeR", "liberator-workspace")
}

.lator_config_path <- function(path) file.path(path, "workspace.json")

.lator_publish_file <- function(temporary, path) {
  backup <- paste0(path, ".previous")
  if (file.exists(backup)) unlink(backup, force = TRUE)
  had_previous <- file.exists(path)
  if (had_previous && !file.rename(path, backup)) .lator_stop("Could not protect the previous workspace file: ", path)
  published <- file.rename(temporary, path)
  if (!published) {
    if (had_previous) file.rename(backup, path)
    .lator_stop("Could not publish workspace file: ", path)
  }
  if (file.exists(backup)) unlink(backup, force = TRUE)
  try(Sys.chmod(path, mode = "0600", use_umask = FALSE), silent = TRUE)
  invisible(path)
}

.lator_write_json <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("lator-", tmpdir = dirname(path), fileext = ".tmp")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value, temporary, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 17
  )
  .lator_publish_file(temporary, path)
}

.lator_derive_key <- function(passphrase, salt, kdf = "argon2id") {
  passphrase <- as.character(passphrase %||% "")
  if (length(passphrase) != 1L || is.na(passphrase) || nchar(passphrase) < 12L) {
    .lator_stop("Workspace passphrases must contain at least 12 characters.")
  }
  bytes <- charToRaw(enc2utf8(passphrase))
  if (identical(kdf, "argon2id")) return(sodium::argon2(bytes, salt = salt, size = 32L))
  if (identical(kdf, "scrypt")) return(sodium::scrypt(bytes, salt = salt, size = 32L))
  .lator_stop("Unsupported workspace key-derivation function: ", kdf)
}

.lator_key_check <- function(key) {
  if (!is.raw(key) || length(key) != 32L) .lator_stop("Workspace keys must contain 32 raw bytes.")
  key
}

.lator_workspace_paths <- function(path) list(
  records = file.path(path, "records"), models = file.path(path, "models"),
  endpoints = file.path(path, "endpoints"), locks = file.path(path, ".locks"),
  catalog = file.path(path, "catalog.enc"), audit = file.path(path, "audit.enc")
)

.lator_workspace_create <- function(path, passphrase = NULL, key = NULL) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  paths <- .lator_workspace_paths(path)
  invisible(lapply(paths[c("records", "models", "endpoints", "locks")], dir.create,
                   recursive = TRUE, showWarnings = FALSE))
  kdf <- "managed-key"
  salt <- raw()
  if (!is.null(key)) {
    key <- .lator_key_check(key)
  } else {
    # Argon2id is preferred. Some Windows libsodium builds cannot reserve its
    # configured memory and report `pwhash failed`; scrypt is the explicit,
    # memory-hard portability fallback and is recorded in the public config.
    salt <- sodium::random(16L)
    key <- tryCatch(.lator_derive_key(passphrase, salt, "argon2id"), error = identity)
    if (inherits(key, "error")) {
      kdf <- "scrypt"
      salt <- sodium::random(32L)
      key <- .lator_derive_key(passphrase, salt, kdf)
    } else kdf <- "argon2id"
  }
  verifier <- sodium::data_tag(charToRaw("LibeRator workspace v1"), key)
  .lator_write_json(list(
    schema = "liberator.workspace", version = 1L, created_at = .lator_now(),
    encryption = "libsodium-xsalsa20-poly1305", kdf = kdf,
    salt = sodium::bin2hex(salt), verifier = sodium::bin2hex(verifier),
    research_only = TRUE, stores_direct_identifiers = FALSE
  ), .lator_config_path(path))
  list(key = key, config = jsonlite::read_json(.lator_config_path(path), simplifyVector = FALSE))
}

.lator_workspace_unlock <- function(path, passphrase = NULL, key = NULL) {
  config <- jsonlite::read_json(.lator_config_path(path), simplifyVector = FALSE)
  if (!identical(config$schema, "liberator.workspace") || as.integer(config$version) != 1L) {
    .lator_stop("Unsupported or invalid LibeRator workspace.")
  }
  salt <- sodium::hex2bin(as.character(config$salt))
  key <- if (!is.null(key)) .lator_key_check(key) else {
    if (identical(config$kdf, "managed-key")) .lator_stop("This workspace requires its managed encryption key.")
    .lator_derive_key(passphrase, salt, as.character(config$kdf))
  }
  verifier <- sodium::data_tag(charToRaw("LibeRator workspace v1"), key)
  if (!identical(sodium::bin2hex(verifier), as.character(config$verifier))) {
    .lator_stop("The workspace passphrase or key is incorrect.")
  }
  list(key = key, config = config)
}

#' Open an encrypted LibeRator workspace
#'
#' Patient records, the patient catalogue, endpoints, model registrations and
#' the audit chain are encrypted and authenticated with libsodium. The key is
#' derived with Argon2 and is never written to the workspace. Direct patient
#' identifiers are deliberately outside the schema; use a pseudonym generated
#' by the controlling study or institution.
#'
#' @param path Workspace directory.
#' @param passphrase Session passphrase of at least 12 characters.
#' @param key Optional 32-byte raw key for managed deployments.
#' @param create Create a missing workspace.
#' @return A key-bearing `lator_workspace`. Do not serialize or transmit it.
#' @export
lator_workspace <- function(path = NULL, passphrase = NULL, key = NULL, create = TRUE) {
  path <- normalizePath(path %||% .lator_default_workspace(), winslash = "/", mustWork = FALSE)
  config_path <- .lator_config_path(path)
  opened <- if (!file.exists(config_path)) {
    if (!isTRUE(create)) .lator_stop("LibeRator workspace does not exist: ", path)
    .lator_workspace_create(path, passphrase = passphrase, key = key)
  } else .lator_workspace_unlock(path, passphrase = passphrase, key = key)
  paths <- .lator_workspace_paths(path)
  invisible(lapply(paths[c("records", "models", "endpoints", "locks")], dir.create,
                   recursive = TRUE, showWarnings = FALSE))
  structure(list(
    schema = "liberator.workspace", version = 1L, path = path,
    key = opened$key, config = opened$config, paths = paths
  ), class = "lator_workspace")
}

#' @export
print.lator_workspace <- function(x, ...) {
  cat("Encrypted LibeRator workspace\n")
  cat("  path:", x$path, "\n")
  cat("  mode: research/teaching; pseudonymous records only\n")
  invisible(x)
}

.lator_require_workspace <- function(workspace) {
  if (!inherits(workspace, "lator_workspace")) .lator_stop("Supply an unlocked `lator_workspace`.")
  .lator_key_check(workspace$key)
  workspace
}

.lator_encrypt <- function(value, key) {
  encrypted <- sodium::data_encrypt(serialize(value, NULL, version = 3L), key)
  list(schema = "liberator.encrypted", version = 1L, payload = encrypted)
}

.lator_decrypt <- function(envelope, key) {
  if (!is.list(envelope) || !identical(envelope$schema, "liberator.encrypted") ||
      as.integer(envelope$version) != 1L || !is.raw(envelope$payload)) {
    .lator_stop("Encrypted workspace record is invalid.")
  }
  unserialize(sodium::data_decrypt(envelope$payload, key))
}

.lator_atomic_encrypt_save <- function(value, path, key) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("lator-", tmpdir = dirname(path), fileext = ".tmp")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(.lator_encrypt(value, key), temporary, version = 3L, compress = "xz")
  .lator_publish_file(temporary, path)
}

.lator_encrypt_read <- function(path, key, default = NULL) {
  if (!file.exists(path)) return(default)
  envelope <- tryCatch(readRDS(path), error = function(error) {
    .lator_stop("Cannot read encrypted record: ", conditionMessage(error))
  })
  tryCatch(.lator_decrypt(envelope, key), error = function(error) {
    .lator_stop("Encrypted record authentication failed: ", conditionMessage(error))
  })
}

.lator_with_lock <- function(workspace, name, operation, timeout = 5) {
  lock <- file.path(workspace$paths$locks, paste0(gsub("[^A-Za-z0-9_-]", "_", name), ".lock"))
  started <- proc.time()[["elapsed"]]
  repeat {
    if (dir.create(lock, showWarnings = FALSE)) break
    if (proc.time()[["elapsed"]] - started >= timeout) .lator_stop("Timed out acquiring workspace lock.")
    Sys.sleep(0.01)
  }
  on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
  operation()
}

.lator_record_token <- function(id, key) {
  sodium::bin2hex(sodium::data_tag(charToRaw(enc2utf8(id)), key))
}

.lator_audit_append <- function(workspace, action, object_type, object_id,
                                detail = list(), actor = "local-session") {
  .lator_require_workspace(workspace)
  .lator_with_lock(workspace, "audit", function() {
    audit <- .lator_encrypt_read(workspace$paths$audit, workspace$key, list(events = list()))
    previous <- if (length(audit$events)) utils::tail(audit$events, 1L)[[1L]]$hash else "GENESIS"
    event <- list(
      id = .lator_id("audit"), at = .lator_now(), actor = as.character(actor),
      action = as.character(action), object_type = as.character(object_type),
      object_token = .lator_record_token(as.character(object_id), workspace$key),
      detail = detail, previous_hash = previous
    )
    event$hash <- .lator_hash(event)
    audit$events <- c(audit$events, list(event))
    .lator_atomic_encrypt_save(audit, workspace$paths$audit, workspace$key)
    invisible(event)
  })
}

#' Read and verify the encrypted workspace audit chain
#' @param workspace Unlocked workspace.
#' @return Audit events with a `valid` attribute.
#' @export
lator_workspace_audit <- function(workspace) {
  workspace <- .lator_require_workspace(workspace)
  audit <- .lator_encrypt_read(workspace$paths$audit, workspace$key, list(events = list()))
  valid <- TRUE
  previous <- "GENESIS"
  for (event in audit$events) {
    hash <- event$hash
    check <- event; check$hash <- NULL
    if (!identical(event$previous_hash, previous) || !identical(hash, .lator_hash(check))) valid <- FALSE
    previous <- hash
  }
  structure(audit$events, valid = valid)
}
