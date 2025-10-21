#' Base Connection Class for Data Sources (top-level)
#'
#' Creates a base connection object that can be extended for different data sources.
#' Placed at top level so devtools::load_all() always sources it.
#'
#' @param source Character string. The data source type (e.g., "redcap")
#' @param url Character string. The API URL
#' @param token Character string. The API token or key
#' @param ssl_verify Logical. Whether to verify SSL certificates (default: TRUE)
#' @param timeout Numeric. Request timeout in seconds (default: 30)
#' @param ... Additional source-specific parameters
#'
#' @return A list object of class '<source>_connection' and 'sardine_connection'
#' @keywords internal
sardine_connection <- function(source, url, token, ssl_verify = TRUE, timeout = 30, ...) {
  if (missing(source) || is.null(source) || source == "") rlang::abort("Data source type is required")
  if (missing(url) || is.null(url) || url == "") rlang::abort("API URL is required")
  if (missing(token) || is.null(token) || token == "") rlang::abort("API token is required")
  if (!is.character(source) || length(source) != 1) rlang::abort("Source must be a single character string")
  if (!is.character(url) || length(url) != 1) rlang::abort("URL must be a single character string")
  if (!is.character(token) || length(token) != 1) rlang::abort("Token must be a single character string")
  if (!is.logical(ssl_verify) || length(ssl_verify) != 1) rlang::abort("ssl_verify must be a single logical value")
  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) rlang::abort("timeout must be a single positive number")

  # Ensure URL ends with '/'
  if (!stringr::str_detect(url, "/$")) url <- paste0(url, "/")

  connection <- list(
    source = source,
    url = url,
    token = token,
    ssl_verify = ssl_verify,
    timeout = timeout,
    created = Sys.time(),
    ...
  )
  class(connection) <- c(paste0(source, "_connection"), "sardine_connection")
  connection
}
