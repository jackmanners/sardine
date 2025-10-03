test_that("redcap_export_arms validates connection object", {
  # Test with invalid connection
  expect_error(
    redcap_export_arms("not_a_connection"),
    "connection must be a redcap_connection object"
  )
})

test_that("redcap_export_arms validates arms parameter", {
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api/",
    token = "test_token"
  )
  
  # Test invalid arms type (should allow character or numeric)
  expect_error(
    redcap_export_arms(conn, arms = list(1, 2)),
    "arms must be a character or numeric vector"
  )
})

test_that("redcap_import_arms validates parameters", {
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api/",
    token = "test_token"
  )
  
  # Test missing data
  expect_error(
    redcap_import_arms(conn),
    "data is required"
  )
  
  # Test NULL data
  expect_error(
    redcap_import_arms(conn, data = NULL),
    "data is required"
  )
})

test_that("redcap_delete_arms validates parameters", {
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api/",
    token = "test_token"
  )
  
  # Test missing arms
  expect_error(
    redcap_delete_arms(conn),
    "arms is required"
  )
  
  # Test invalid arms type
  expect_error(
    redcap_delete_arms(conn, arms = list(1, 2)),
    "arms must be a character or numeric vector"
  )
})

test_that("redcap_export_events validates arms parameter", {
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api/",
    token = "test_token"
  )
  
  # Test invalid arms type
  expect_error(
    redcap_export_events(conn, arms = list(1, 2)),
    "arms must be a character or numeric vector"
  )
})

test_that("redcap_delete_events validates parameters", {
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api/",
    token = "test_token"
  )
  
  # Test missing events
  expect_error(
    redcap_delete_events(conn),
    "events is required"
  )
  
  # Test invalid events type
  expect_error(
    redcap_delete_events(conn, events = 123),
    "events must be a character vector"
  )
})