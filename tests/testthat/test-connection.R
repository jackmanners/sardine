# Note: redcap_project() tests require a real REDCap instance
# These are integration tests and should be skipped in CI unless credentials are available

test_that("redcap_project validates required parameters", {
  # Test missing URL and no env vars
  expect_error(
    withr::with_envvar(c(REDCAP_URL = NA, REDCAP_TOKEN = NA), {
      redcap_project()
    }),
    "URL must be provided"
  )
  
  # Test missing token and no env vars
  expect_error(
    withr::with_envvar(c(REDCAP_URL = "https://example.com/api/", REDCAP_TOKEN = NA), {
      redcap_project()
    }),
    "Token must be provided"
  )
})

test_that("redcap_project uses environment variables as fallback", {
  skip_if_not(Sys.getenv("REDCAP_URL") != "" && Sys.getenv("REDCAP_TOKEN") != "",
              "REDCap credentials not available in environment")
  
  # If env vars are set, this should work
  expect_no_error(
    project <- redcap_project()
  )
})

test_that("redcap_project creates valid project object", {
  skip_if_not(Sys.getenv("REDCAP_URL") != "" && Sys.getenv("REDCAP_TOKEN") != "",
              "REDCap credentials not available in environment")
  
  project <- redcap_project()
  
  expect_s3_class(project, "redcap_project")
  expect_true("data" %in% names(project))
  expect_true("metadata" %in% names(project))
  expect_true("project_info" %in% names(project))
  expect_true(is.function(project$refresh))
  expect_true(is.function(project$info))
})

test_that("print.redcap_project works correctly", {
  skip_if_not(Sys.getenv("REDCAP_URL") != "" && Sys.getenv("REDCAP_TOKEN") != "",
              "REDCap credentials not available in environment")
  
  project <- redcap_project()
  
  # Capture the output
  output <- capture.output(print(project))
  
  expect_true(any(grepl("REDCap Project", output)))
  expect_true(any(grepl("Title:", output)))
  expect_true(any(grepl("Cached Data:", output)))
})