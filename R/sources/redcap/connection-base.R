#' Base Connection Class for Data Sources
#'
#' @description
#' Creates a base connection object that can be extended for different data sources.
#' This provides a consistent interface across different APIs while allowing for
#' source-specific implementations.
#'
#' @param source Character string. The data source type (e.g., "redcap", "qualtrics", etc.)
#' @param url Character string. The API URL
#' @param token Character string. The API token or key
#' @param ssl_verify Logical. Whether to verify SSL certificates (default: TRUE)
#' @param timeout Numeric. Request timeout in seconds (default: 30)
#' @param ... Additional source-specific parameters
#'
#' @return A list object of class 'sardine_connection' with the specified source subclass
#'
#' @examples
#' \dontrun{
#' # This is typically called by source-specific functions like redcap_connection()
#' conn <- sardine_connection(
#'   source = "redcap",
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#' }
#'
#' @export
sardine_connection <- function(source, url, token, ssl_verify = TRUE, timeout = 30, ...) {
  # Input validation
  if (missing(source) || is.null(source) || source == "") {
    rlang::abort("Data source type is required")
  }
  
  if (missing(url) || is.null(url) || url == "") {
    rlang::abort("API URL is required")
  }
  
  if (missing(token) || is.null(token) || token == "") {
    rlang::abort("API token is required")
  }
  
  if (!is.character(source) || length(source) != 1) {
    rlang::abort("Source must be a single character string")
  }
  
  if (!is.character(url) || length(url) != 1) {
    rlang::abort("URL must be a single character string")
  }
  
  if (!is.character(token) || length(token) != 1) {
    rlang::abort("Token must be a single character string")
  }
  
  if (!is.logical(ssl_verify) || length(ssl_verify) != 1) {
    rlang::abort("ssl_verify must be a single logical value")
  }
  
  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    rlang::abort("timeout must be a single positive number")
  }
  
  # Ensure URL ends with '/' for consistency
  if (!stringr::str_detect(url, "/$")) {
    url <- paste0(url, "/")
  }
  
  # Create connection object
  connection <- list(
    source = source,
    url = url,
    token = token,
    ssl_verify = ssl_verify,
    timeout = timeout,
    created = Sys.time(),
    ...
  )
  
  # Set class hierarchy
  class(connection) <- c(paste0(source, "_connection"), "sardine_connection")
  
  return(connection)
}

#' Print method for sardine connection
#'
#' @param x A sardine_connection object
#' @param ... Additional arguments (not used)
#'
#' @exportS3method print sardine_connection
print.sardine_connection <- function(x, ...) {
  cat("Sardine Connection Object\n")
  cat("=========================\n")
  cat("Source:  ", toupper(x$source), "\n")
  cat("URL:     ", x$url, "\n")
  cat("Token:   ", paste0(substr(x$token, 1, 8), "..."), "\n")
  cat("SSL:     ", x$ssl_verify, "\n")
  cat("Timeout: ", x$timeout, " seconds\n")
  cat("Created: ", format(x$created), "\n")
  invisible(x)
}

#' Test connection to any data source
#'
#' @description
#' Generic function to test connections to various data sources. Dispatches
#' to source-specific testing methods.
#'
#' @param connection A sardine_connection object
#'
#' @return Logical. TRUE if connection is successful, FALSE otherwise
#'
#' @export
test_connection <- function(connection) {
  UseMethod("test_connection")
}

#' Default test connection method
#' @export
test_connection.default <- function(connection) {
  rlang::abort("test_connection not implemented for this connection type")
}