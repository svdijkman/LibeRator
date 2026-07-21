test_that("hosted dosing GUI uses a separate workspace path per session", {
  root <- tempfile("lator-hosted-")
  app <- lator_gui(
    path = root, session_workspace = TRUE, launch.browser = NULL
  )
  expect_s3_class(app, "shiny.appobj")
  server <- app[["serverFuncSource"]]()
  paths <- character()
  shiny::testServer(server, {
    paths <<- c(paths, session_path)
  })
  shiny::testServer(server, {
    paths <<- c(paths, session_path)
  })
  expect_length(unique(paths), 2L)
  expect_true(all(startsWith(
    normalizePath(paths, winslash = "/", mustWork = FALSE),
    paste0(normalizePath(root, winslash = "/", mustWork = FALSE), "/sessions/")
  )))
})
