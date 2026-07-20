contains_raw <- function(path, text) {
  haystack <- readBin(path, "raw", n = file.info(path)$size)
  needle <- charToRaw(text)
  if (length(haystack) < length(needle)) return(FALSE)
  any(vapply(seq_len(length(haystack) - length(needle) + 1L), function(index) {
    identical(haystack[index + seq_along(needle) - 1L], needle)
  }, logical(1)))
}

test_that("encrypted workspace persists pseudonymous records and audit chain", {
  root <- tempfile("lator-workspace-")
  workspace <- lator_workspace(root, "correct horse battery staple")
  patient <- lator_patient_save(workspace, lator_patient_new("SECRET-PSEUDONYM"))
  expect_equal(patient$revision, 1L)
  expect_equal(lator_patient_get(workspace, "SECRET-PSEUDONYM")$patient_id, "SECRET-PSEUDONYM")
  expect_false(contains_raw(workspace$paths$catalog, "SECRET-PSEUDONYM"))
  record <- list.files(workspace$paths$records, full.names = TRUE)
  expect_false(contains_raw(record[[1L]], "SECRET-PSEUDONYM"))
  expect_true(isTRUE(attr(lator_workspace_audit(workspace), "valid")))
  expect_error(lator_workspace(root, "this is the wrong passphrase", create = FALSE), "incorrect|failed")
  reopened <- lator_workspace(root, "correct horse battery staple", create = FALSE)
  expect_equal(lator_patient_list(reopened)$patient_id, "SECRET-PSEUDONYM")
})

test_that("managed-key workspaces never require a persisted passphrase", {
  root <- tempfile("lator-managed-")
  key <- sodium::random(32)
  workspace <- lator_workspace(root, key = key)
  reopened <- lator_workspace(root, key = key, create = FALSE)
  expect_s3_class(reopened, "lator_workspace")
  expect_error(lator_workspace(root, passphrase = "a passphrase that is long enough", create = FALSE), "managed")
})

test_that("optimistic revisions prevent overwriting another session", {
  workspace <- lator_workspace(tempfile("lator-revision-"), "revision test passphrase")
  original <- lator_patient_save(workspace, lator_patient_new("P001"))
  first <- lator_patient_add_event(original, "note", 1, value = "first")
  lator_patient_save(workspace, first)
  stale <- lator_patient_add_event(original, "note", 2, value = "stale")
  expect_error(lator_patient_save(workspace, stale), "revision conflict")
})
