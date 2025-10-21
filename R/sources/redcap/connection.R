#' REDCap Connection
#'
#' @description
#' Creates a connection to a REDCap project API. This is a wrapper around
#' the base source_connection that provides REDCap-specific functionality.
#'
#' @param url Character string. The REDCap API URL (e.g., "https://redcap.example.edu/api/")
#' @param token Character string. Your REDCap API token
#' @param ssl_verify Logical. Whether to verify SSL certificates (default: TRUE)
#' @param timeout Numeric. Request timeout in seconds (default: 30)
#'
#' @return A redcap_connection object that inherits from source_connection
#'
#' @examples
#' \dontrun{
#' # Basic connection
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Using environment variables
#' conn <- redcap_connection(
#'   url = Sys.getenv("REDCAP_URL"),
#'   token = Sys.getenv("REDCAP_TOKEN")
#' )
#' }
#'
#' @export
redcap_connection <- function(url, token, ssl_verify = TRUE, timeout = 30) {
  
  # Validate REDCap-specific requirements
  if (!stringr::str_detect(url, "/api/?$")) {
    rlang::warn("URL should end with '/api/' for REDCap connections")
  }
  
  # Create base connection using the source_connection class
  connection <- source_connection(
    source_type = "redcap",
    url = url,
    token = token,
    ssl_verify = ssl_verify,
    timeout = timeout
  )
  
  return(connection)
}

#' Test REDCap Connection
#'
#' @description
#' Tests the REDCap connection by making a simple API request to verify
#' that the URL and token are valid and the API is accessible.
#'
#' @param connection A redcap_connection object created by \code{\link{redcap_connection}}
#'
#' @return Logical. TRUE if connection is successful, FALSE otherwise
#'
#' @export
test_connection.redcap_connection <- function(connection, ...) {
  tryCatch({
    # Make a simple request to get project information
    response <- .redcap_request(
      connection = connection,
      content = "project",
      return_format = "json"
    )
    
    if (httr2::resp_is_error(response)) {
      cli::cli_alert_danger("Connection test failed: HTTP error {httr2::resp_status(response)}")
      return(FALSE)
    }
    
    # Try to parse the response
    project_info <- httr2::resp_body_json(response)
    
    if (is.null(project_info$project_title)) {
      cli::cli_alert_warning("Connection successful but unexpected response format")
      return(FALSE)
    }
    
    cli::cli_alert_success("Connection test successful!")
    cli::cli_alert_info("Project: {project_info$project_title}")
    
    return(TRUE)
    
  }, error = function(e) {
    cli::cli_alert_danger("Connection test failed: {e$message}")
    return(FALSE)
  })
}