test_that("models and endpoints are registered only inside encrypted workspace", {
  workspace <- lator_workspace(tempfile("lator-registry-"), "registry test passphrase")
  endpoint <- lator_endpoint_aed("Drug A", 1, 4, "mg/L", "teaching source")
  expect_type(lator_model_register(workspace, lator_test_model(), "model-a"), "list")
  expect_s3_class(lator_endpoint_register(workspace, endpoint), "lator_endpoint")
  expect_s3_class(LibeRator:::.lator_model_get(workspace, "model-a")$model, "nm_model")
  expect_s3_class(LibeRator:::.lator_endpoint_get(workspace, endpoint$id), "lator_endpoint")
})

test_that("queue jobs reject apparent direct identifiers", {
  skip_if_not_installed("LibeRties", minimum_version = "0.6.1")
  data <- data.frame(ID = "PSEUDONYM", TIME = 0, EVID = 1, AMT = 100)
  job <- lator_job("individualise", lator_test_model(), data)
  expect_s3_class(job, "liber_job")
  expect_identical(job$type, "individualise")
  expect_error(lator_job("individualise", lator_test_model(), transform(data, PATIENT_NAME = "A")), "identifier")
})
