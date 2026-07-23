test_that("therapeutic GUI retains shared theme, dove, and responsive controls", {
  script <- paste(readLines(
    system.file("htmlwidgets", "liberatorWorkbench.js", package = "LibeRator"),
    warn = FALSE
  ), collapse = "\n")
  base_css <- paste(readLines(
    system.file("htmlwidgets", "liberatorWorkbench.css", package = "LibeRator"),
    warn = FALSE
  ), collapse = "\n")
  extras_css <- paste(readLines(
    system.file("htmlwidgets", "liberatorExtras.css", package = "LibeRator"),
    warn = FALSE
  ), collapse = "\n")

  expect_match(script, 'localStorage\\.getItem\\("liber\\.theme"\\)')
  favicon <- paste(readLines(
    system.file("assets", "favicon.svg", package = "LibeRator"),
    warn = FALSE
  ), collapse = "")
  expect_match(favicon, 'id="liberator-dove"', fixed = TRUE)
  expect_match(favicon, "data:image/png;base64,", fixed = TRUE)
  expect_match(script, "lr-sidebar-toggle", fixed = TRUE)
  expect_match(script, "lr-rail-toggle", fixed = TRUE)
  expect_match(script, "useDialogFocus", fixed = TRUE)
  expect_match(base_css, "--brand:", fixed = TRUE)
  expect_false(grepl("--purple", paste(base_css, extras_css)))
  expect_match(
    base_css,
    "grid-template-rows:58px 32px minmax(0,1fr) 27px",
    fixed = TRUE
  )
  expect_match(base_css, ".lr-logo{width:42px;height:42px", fixed = TRUE)
  expect_match(base_css, ".lr-button{min-height:32px", fixed = TRUE)
  expect_match(base_css, ".lr-panel{margin-bottom:10px;border:1px solid var(--line);border-radius:10px", fixed = TRUE)
  expect_match(extras_css, "\\.lr-sidebar\\.open")
  expect_match(extras_css, "\\.lr-right\\.open")
})
