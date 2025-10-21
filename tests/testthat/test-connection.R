# Note: Tests use a hardcoded test project to avoid testing on production

# Test credentials for dedicated testing project
TEST_URL <- "https://researchsurvey.flinders.edu.au/api/"
TEST_TOKEN <- "6DB3351D46845FB617675D4AECC7B211"

test_that("redcap_project validates required parameters", {
  # Test missing token (empty string)
  expect_error(
    redcap_project(url = TEST_URL, token = ""),
    "token"
  )
})

test_that("redcap_project creates valid project object", {
  project <- redcap_project(url = TEST_URL, token = TEST_TOKEN)
  
  expect_s3_class(project, "redcap_project")
  expect_true("data" %in% names(project))
  expect_true("metadata" %in% names(project))
  expect_true("project_info" %in% names(project))
  expect_true(is.function(project$refresh))
  expect_true(is.function(project$info))
})

test_that("print.redcap_project works correctly", {
  project <- redcap_project(url = TEST_URL, token = TEST_TOKEN)
  
  # Capture the output
  output <- capture.output(print(project))
  
  expect_true(any(grepl("REDCap Project", output)))
  expect_true(any(grepl("Title:", output)))
  expect_true(any(grepl("Records:", output)))
})