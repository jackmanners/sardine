test_that("redcap_import_records validates connection object", {
  # Test with invalid connection
  expect_error(
    redcap_import_records("not_a_connection", data.frame(record_id = "001")),
    "connection must be a redcap_connection object"
  )
})

test_that("redcap_import_records validates data parameter", {
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api/",
    token = "test_token"
  )
  
  # Test missing data
  expect_error(
    redcap_import_records(conn),
    "data is required"
  )
  
  # Test NULL data
  expect_error(
    redcap_import_records(conn, data = NULL),
    "data is required"
  )
})

test_that("redcap_delete_records validates parameters", {
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api/",
    token = "test_token"
  )
  
  # Test missing records
  expect_error(
    redcap_delete_records(conn),
    "records is required"
  )
  
  # Test invalid records type
  expect_error(
    redcap_delete_records(conn, records = 123),
    "records must be a character vector"
  )
  
  # Test invalid arm type
  expect_error(
    redcap_delete_records(conn, records = "001", arm = "not_numeric"),
    "arm must be a single numeric value"
  )
})

test_that("redcap_rename_record validates parameters", {
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api/",
    token = "test_token"
  )
  
  # Test missing parameters
  expect_error(
    redcap_rename_record(conn, old_id = "001"),
    "Both old_id and new_id are required"
  )
  
  expect_error(
    redcap_rename_record(conn, new_id = "002"),
    "Both old_id and new_id are required"
  )
  
  # Test invalid parameter types
  expect_error(
    redcap_rename_record(conn, old_id = 123, new_id = "002"),
    "old_id must be a single character string"
  )
  
  expect_error(
    redcap_rename_record(conn, old_id = "001", new_id = 456),
    "new_id must be a single character string"
  )
})