test_that("LibeRator renders an isolated teaching session in a real browser", {
  skip_if_not_installed("shinytest2")
  skip_if(Sys.getenv("LIBER_RUN_BROWSER_TESTS") != "true")
  app <- LibeRator::lator_gui(
    session_workspace = TRUE, teaching_example = TRUE, launch.browser = NULL
  )
  driver <- shinytest2::AppDriver$new(
    app, name = "liberator-browser", width = 1366, height = 768,
    load_timeout = 120000, seed = 20260723
  )
  on.exit(driver$stop(), add = TRUE)
  driver$wait_for_idle()
  expect_identical(driver$get_js("document.title"), "LibeRator")
  expect_match(driver$get_js("document.body.innerText"), "Unlock workspace")
  expect_false(driver$get_js(
    "document.documentElement.scrollWidth > document.documentElement.clientWidth + 2"
  ))
})
