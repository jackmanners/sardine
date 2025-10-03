test_that("redcap_connection creates valid connection object", {
  # Test basic connection creation
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api/",
    token = "test_token_123456789"
  )
  
  expect_s3_class(conn, c("redcap_connection", "sardine_connection"))
  expect_equal(conn$source, "redcap")
  expect_equal(conn$url, "https://redcap.example.edu/api/")
  expect_equal(conn$token, "test_token_123456789")
  expect_true(conn$ssl_verify)
  expect_equal(conn$timeout, 30)
  expect_true(inherits(conn$created, "POSIXct"))
})

test_that("redcap_connection handles URL formatting", {
  # Test URL without trailing slash
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api",
    token = "test_token"
  )
  
  expect_equal(conn$url, "https://redcap.example.edu/api/")
})

test_that("redcap_connection validates inputs", {
  # Test missing URL
  expect_error(
    redcap_connection(token = "test_token"),
    "API URL is required"
  )
  
  # Test missing token
  expect_error(
    redcap_connection(url = "https://redcap.example.edu/api/"),
    "API token is required"
  )
  
  # Test empty URL
  expect_error(
    redcap_connection(url = "", token = "test_token"),
    "API URL is required"
  )
  
  # Test empty token
  expect_error(
    redcap_connection(url = "https://redcap.example.edu/api/", token = ""),
    "API token is required"
  )
  
  # Test invalid URL type
  expect_error(
    redcap_connection(url = 123, token = "test_token"),
    "URL must be a single character string"
  )
  
  # Test invalid token type
  expect_error(
    redcap_connection(url = "https://redcap.example.edu/api/", token = 123),
    "Token must be a single character string"
  )
  
  # Test invalid ssl_verify
  expect_error(
    redcap_connection(
      url = "https://redcap.example.edu/api/", 
      token = "test_token", 
      ssl_verify = "yes"
    ),
    "ssl_verify must be a single logical value"
  )
  
  # Test invalid timeout
  expect_error(
    redcap_connection(
      url = "https://redcap.example.edu/api/", 
      token = "test_token", 
      timeout = -1
    ),
    "timeout must be a single positive number"
  )
})

test_that("print.sardine_connection works correctly", {
  conn <- redcap_connection(
    url = "https://redcap.example.edu/api/",
    token = "test_token_123456789"
  )
  
  # Capture the output
  output <- capture.output(print(conn))
  
  expect_true(any(grepl("Sardine Connection Object", output)))
  expect_true(any(grepl("REDCAP", output)))
  expect_true(any(grepl("https://redcap.example.edu/api/", output)))
  expect_true(any(grepl("test_tok...", output))) # Token should be masked
})