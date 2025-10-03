test_that("redcap_export_records validates connection object", {
  # Test with invalid connection
  expect_error(
    redcap_export_records("not_a_connection"),
    "connection must be a redcap_connection object"
  )
})

test_that("redcap_export_records validates parameters", {
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api/",
    token = "test_token"
  )
  
  # Test invalid raw_or_label
  expect_error(
    redcap_export_records(conn, raw_or_label = "invalid"),
    "raw_or_label must be either 'raw' or 'label'"
  )
  
  # Test invalid raw_or_label_headers
  expect_error(
    redcap_export_records(conn, raw_or_label_headers = "invalid"),
    "raw_or_label_headers must be either 'raw' or 'label'"
  )
  
  # Test invalid return_format
  expect_error(
    redcap_export_records(conn, return_format = "invalid"),
    "return_format must be 'json', 'csv', or 'xml'"
  )
  
  # Test invalid records parameter
  expect_error(
    redcap_export_records(conn, records = 123),
    "records must be a character vector"
  )
  
  # Test invalid fields parameter
  expect_error(
    redcap_export_records(conn, fields = 123),
    "fields must be a character vector"
  )
  
  # Test invalid forms parameter
  expect_error(
    redcap_export_records(conn, forms = 123),
    "forms must be a character vector"
  )
  
  # Test invalid events parameter
  expect_error(
    redcap_export_records(conn, events = 123),
    "events must be a character vector"
  )
  
  # Test invalid filter_logic parameter
  expect_error(
    redcap_export_records(conn, filter_logic = c("logic1", "logic2")),
    "filter_logic must be a single character string"
  )
})