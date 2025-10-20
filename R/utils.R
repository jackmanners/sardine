#' Test REDCap API Connection
#'
#' @description
#' Tests the connection to REDCap API by making a simple request to verify
#' that the URL and token are valid and the API is accessible.
#'
#' @param connection A redcap_connection object created by \code{\link{redcap_connection}}
#'
#' @return Logical. TRUE if connection is successful, FALSE otherwise
#'
#' @export
#' @rdname test_connection
#' @examples
#' \dontrun{
#'   conn <- redcap_connection(url = "https://example/api/", token = "TOKEN")
#'   ok <- test_connection(conn)
#' }

test_connection <- function(connection, ...) {
  UseMethod("test_connection")
}

#' @export
test_connection.default <- function(connection, ...) {
  rlang::abort("No test_connection method for objects of this class")
}

#' @export
test_connection.redcap_connection <- function(connection) {
  tryCatch({
    # Make a simple request to get project information
    response <- .redcap_request(
      connection = connection,
      content = "project",
      format = "json"
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

#' Internal function to make REDCap API requests
#'
#' @param connection A redcap_connection object
#' @param content Character string specifying the content type
#' @param format Character string specifying the response format
#' @param ... Additional parameters to pass to the API
#'
#' @return httr2 response object
#' @keywords internal
.redcap_request <- function(connection, content, format = "json", ...) {
  # Prepare the request body
  body_params <- list(
    token = connection$token,
    content = content,
    format = format,
    ...
  )
  
  # Remove NULL parameters
  body_params <- body_params[!sapply(body_params, is.null)]
  
  # Create and perform the request
  request <- httr2::request(connection$url) |>
    httr2::req_headers(
      "Content-Type" = "application/x-www-form-urlencoded",
      "User-Agent" = paste0("sardine/", utils::packageVersion("sardine"), " (R)")
    ) |>
    httr2::req_body_form(!!!body_params)
  
  # Set timeout if specified
  if (!is.null(connection$timeout)) {
    # Note: httr2 handles timeouts differently, using built-in timeout functionality
  }
  
  # Perform the request
  response <- httr2::req_perform(request)
  
  return(response)
}