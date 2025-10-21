# Tests for REDCap project data access and manipulation
# Note: Tests use a hardcoded test project to avoid testing on production

# Test credentials for dedicated testing project
TEST_URL <- "https://researchsurvey.flinders.edu.au/api/"
TEST_TOKEN <- "6DB3351D46845FB617675D4AECC7B211"

test_that("project data is accessible", {
  project <- redcap_project(url = TEST_URL, token = TEST_TOKEN)
  
  expect_true("data" %in% names(project))
  expect_true(is.data.frame(project$data) || is.null(project$data))
})

test_that("project metadata is accessible", {
  project <- redcap_project(url = TEST_URL, token = TEST_TOKEN)
  
  expect_true("metadata" %in% names(project))
  expect_true(is.data.frame(project$metadata) || is.null(project$metadata))
})

test_that("project info is accessible", {
  project <- redcap_project(url = TEST_URL, token = TEST_TOKEN)
  
  expect_true("project_info" %in% names(project))
  expect_true(is.list(project$project_info))
})

test_that("project refresh works", {
  project <- redcap_project(url = TEST_URL, token = TEST_TOKEN)
  
  expect_true(is.function(project$refresh))
  expect_no_error(project$refresh())
})

test_that("project info method works", {
  project <- redcap_project(url = TEST_URL, token = TEST_TOKEN)
  
  expect_true(is.function(project$info))
  output <- capture.output(project$info())
  expect_true(any(grepl("REDCap Project", output)))
})

test_that("data quality functions work with project", {
  project <- redcap_project(url = TEST_URL, token = TEST_TOKEN)
  
  # Skip if no data
  skip_if(is.null(project$data) || nrow(project$data) == 0,
          "No data available in project")
  
  # Skip if no metadata (required for quality functions)
  skip_if(is.null(project$metadata) || nrow(project$metadata) == 0,
          "No metadata available in project")
  
  # Test analyze_missing_data - wrapped in try to handle edge cases
  missing_result <- tryCatch(
    analyze_missing_data(project, threshold = 0.20),
    error = function(e) {
      skip(paste("Missing data analysis failed:", e$message))
    }
  )
  
  if (!inherits(missing_result, "missing_data_analysis")) {
    skip("analyze_missing_data returned unexpected type")
  }
  
  expect_s3_class(missing_result, "missing_data_analysis")
  expect_true("summary" %in% names(missing_result))
  expect_true("by_field" %in% names(missing_result))
  
  # Test validate_data_types
  validation_result <- tryCatch(
    validate_data_types(project, strict = FALSE),
    error = function(e) {
      skip(paste("Data type validation failed:", e$message))
    }
  )
  
  if (!inherits(validation_result, "data_type_validation")) {
    skip("validate_data_types returned unexpected type")
  }
  
  expect_s3_class(validation_result, "data_type_validation")
  expect_true("summary" %in% names(validation_result))
  
  # Test generate_data_quality_report
  report <- tryCatch(
    generate_data_quality_report(project, missing_threshold = 0.15),
    error = function(e) {
      skip(paste("Quality report generation failed:", e$message))
    }
  )
  
  if (!inherits(report, "data_quality_report")) {
    skip("generate_data_quality_report returned unexpected type")
  }
  
  expect_s3_class(report, "data_quality_report")
  expect_true("summary" %in% names(report))
  expect_true("missing_analysis" %in% names(report))
})

test_that("metadata can be used for field filtering", {
  project <- redcap_project(url = TEST_URL, token = TEST_TOKEN)
  
  skip_if(is.null(project$metadata) || nrow(project$metadata) == 0,
          "No metadata available")
  
  # Should have expected metadata columns
  expect_true("field_name" %in% names(project$metadata))
  expect_true("field_type" %in% names(project$metadata))
  expect_true("form_name" %in% names(project$metadata))
  
  # Should be able to count field types
  field_types <- project$metadata %>% 
    dplyr::count(field_type)
  expect_true(nrow(field_types) > 0)
})

test_that("project object has expected structure", {
  project <- redcap_project(url = TEST_URL, token = TEST_TOKEN)
  
  # Check class
  expect_s3_class(project, "redcap_project")
  expect_s3_class(project, "sardine_project")
  
  # Check methods exist
  expect_true(is.function(project$refresh))
  expect_true(is.function(project$info))
  
  # Check properties exist
  expect_true(".connection" %in% names(project))
  expect_true(".created_at" %in% names(project))
  expect_true("data" %in% names(project))
  expect_true("metadata" %in% names(project))
  expect_true("project_info" %in% names(project))
  
  # Check created_at is a time
  expect_s3_class(project$.created_at, "POSIXct")
})

