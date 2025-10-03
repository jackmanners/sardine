#' Base Source Connection Class
#'
#' @description
#' Base class for API source connections in the sardine package.
#' This provides a common interface that all specific source connections
#' (REDCap, Qualtrics, etc.) can inherit from.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{test_connection()}}{Test if the connection is working}
#'   \item{\code{get_connection_info()}}{Get basic connection information}
#'   \item{\code{is_connected()}}{Check connection status}
#' }
#'
#' @examples
#' \dontrun{
#' # This is a base class - use specific implementations like:
#' conn <- redcap_connection(url = "...", token = "...")
#' }
#'
#' @export
source_connection <- function(source_type, url, token, ssl_verify = TRUE, timeout = 30, ...) {
  
  # Validate required parameters
  if (missing(source_type) || is.null(source_type) || source_type == "") {
    stop("source_type is required and cannot be empty")
  }
  
  if (missing(url) || is.null(url) || url == "") {
    stop("url is required and cannot be empty")
  }
  
  if (missing(token) || is.null(token) || token == "") {
    stop("token is required and cannot be empty")
  }
  
  # Create connection object
  connection <- list(
    source_type = source_type,
    url = url,
    token = token,
    ssl_verify = ssl_verify,
    timeout = timeout,
    created_at = Sys.time(),
    additional_params = list(...)
  )
  
  # Set class hierarchy
  class(connection) <- c(paste0(source_type, "_connection"), "source_connection")
  
  return(connection)
}

#' Test Connection (Generic)
#'
#' @description
#' Generic method for testing API connections. Specific implementations
#' should provide their own methods for different source types.
#'
#' @param connection A source_connection object
#'
#' @return Logical. TRUE if connection successful, FALSE otherwise
#'
#' @export
test_connection <- function(connection) {
  UseMethod("test_connection")
}

#' Default Test Connection Method
#'
#' @description
#' Default implementation that provides basic URL accessibility testing.
#' Source-specific implementations should override this method.
#'
#' @param connection A source_connection object
#'
#' @return Logical. TRUE if basic URL is accessible, FALSE otherwise
#'
#' @export
test_connection.default <- function(connection) {
  
  if (!inherits(connection, "source_connection")) {
    cli::cli_alert_danger("Object is not a valid source_connection")
    return(FALSE)
  }
  
  cli::cli_alert_warning("Using default connection test for {connection$source_type}")
  cli::cli_alert_info("Consider implementing a specific test_connection.{connection$source_type}_connection method")
  
  tryCatch({
    # Basic URL accessibility test
    response <- httr2::request(connection$url) %>%
      httr2::req_timeout(connection$timeout) %>%
      httr2::req_perform()
    
    if (httr2::resp_is_error(response)) {
      cli::cli_alert_danger("URL not accessible: HTTP {httr2::resp_status(response)}")
      return(FALSE)
    }
    
    cli::cli_alert_success("Basic URL accessibility test passed")
    return(TRUE)
    
  }, error = function(e) {
    cli::cli_alert_danger("Connection test failed: {e$message}")
    return(FALSE)
  })
}

#' Get Connection Information
#'
#' @description
#' Returns basic information about a source connection without
#' exposing sensitive details like tokens.
#'
#' @param connection A source_connection object
#'
#' @return A list with connection information
#'
#' @export
get_connection_info <- function(connection) {
  
  if (!inherits(connection, "source_connection")) {
    stop("Object is not a valid source_connection")
  }
  
  info <- list(
    source_type = connection$source_type,
    url = connection$url,
    ssl_verify = connection$ssl_verify,
    timeout = connection$timeout,
    created_at = connection$created_at,
    class = class(connection),
    token_length = nchar(connection$token),
    has_additional_params = length(connection$additional_params) > 0
  )
  
  return(info)
}

#' Check if Connection is Valid
#'
#' @description
#' Performs basic validation checks on a connection object to ensure
#' it has all required components.
#'
#' @param connection A source_connection object
#'
#' @return Logical. TRUE if connection object is valid, FALSE otherwise
#'
#' @export
is_valid_connection <- function(connection) {
  
  if (!inherits(connection, "source_connection")) {
    return(FALSE)
  }
  
  required_fields <- c("source_type", "url", "token", "ssl_verify", "timeout", "created_at")
  
  missing_fields <- setdiff(required_fields, names(connection))
  
  if (length(missing_fields) > 0) {
    cli::cli_alert_warning("Connection missing required fields: {paste(missing_fields, collapse = ', ')}")
    return(FALSE)
  }
  
  # Check for empty critical fields
  if (connection$source_type == "" || connection$url == "" || connection$token == "") {
    cli::cli_alert_warning("Connection has empty critical fields")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Print Method for Source Connections
#'
#' @description
#' Custom print method for source_connection objects that shows
#' useful information without exposing sensitive data.
#'
#' @param x A source_connection object
#' @param ... Additional arguments (unused)
#'
#' @export
print.source_connection <- function(x, ...) {
  
  cat("Sardine API Connection\n")
  cat("======================\n\n")
  
  cat("Source Type:", x$source_type, "\n")
  cat("URL:", x$url, "\n")
  cat("Token:", paste0(substr(x$token, 1, 8), "..."), "(", nchar(x$token), " characters)\n")
  cat("SSL Verify:", x$ssl_verify, "\n")
  cat("Timeout:", x$timeout, "seconds\n")
  cat("Created:", format(x$created_at, "%Y-%m-%d %H:%M:%S"), "\n")
  
  if (length(x$additional_params) > 0) {
    cat("Additional Parameters:", length(x$additional_params), "items\n")
  }
  
  cat("\nClass:", paste(class(x), collapse = " -> "), "\n")
  
  invisible(x)
}