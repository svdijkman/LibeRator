#' LibeRator: Adaptive Therapeutic Optimisation and Recommendation
#'
#' LibeRator is a research and teaching package. Its model-based outputs are
#' hypotheses for review by qualified professionals, not autonomous treatment
#' instructions or validated clinical recommendations.
#'
#' @keywords internal
"_PACKAGE"

`%||%` <- function(x, y) if (is.null(x)) y else x

.lator_stop <- function(...) stop(..., call. = FALSE)

.lator_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

.lator_id <- function(prefix = "id") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d%H%M%OS3", tz = "UTC"), "_",
         substr(paste0(sodium::bin2hex(sodium::random(8L))), 1L, 16L))
}

.lator_scalar <- function(value, what, allow_empty = FALSE, max_chars = 256L) {
  value <- as.character(value %||% "")
  if (length(value) != 1L || is.na(value) || grepl("[\r\n\t]", value) ||
      nchar(value, type = "chars") > max_chars || (!allow_empty && !nzchar(trimws(value)))) {
    .lator_stop("`", what, "` must be one valid single-line value.")
  }
  trimws(value)
}

.lator_number <- function(value, what, finite = TRUE, positive = FALSE) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value) || (finite && !is.finite(value)) ||
      (positive && value <= 0)) .lator_stop("`", what, "` is invalid.")
  value
}

.lator_hash <- function(value) {
  digest::digest(serialize(value, NULL, version = 3L), algo = "sha256", serialize = FALSE)
}
