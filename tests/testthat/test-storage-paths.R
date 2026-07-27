test_that("LibeRator supports an explicit patient-data root", {
  root <- file.path(tempdir(), paste0("liberator-home-", Sys.getpid()))
  old <- Sys.getenv("LIBERATOR_HOME", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("LIBERATOR_HOME")
    else Sys.setenv(LIBERATOR_HOME = old)
    unlink(root, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  Sys.setenv(LIBERATOR_HOME = root)
  expect_equal(
    .lator_default_workspace(),
    normalizePath(root, winslash = "/", mustWork = FALSE)
  )
})
