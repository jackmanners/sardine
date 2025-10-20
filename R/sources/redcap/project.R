#' Internal REDCap Project Creation Function
#'
#' @description
#' Internal function that creates a REDCap project object. Users should use 
#' the main redcap_project() function instead of calling this directly.
#'
#' @param url Character string. The REDCap API URL (e.g., "https://redcap.example.edu/api/")
#' @param token Character string. Your REDCap API token
#' @param ssl_verify Logical. Whether to verify SSL certificates (default: TRUE)
#' @param timeout Numeric. Request timeout in seconds (default: 30)
#'
#' @return A redcap_project object with cached data and methods
#' @keywords internal
NULL

#' Create REDCap Project from Environment Variables
#'
#' @description


# Internal helper functions
.redcap_get_project_info <- function(connection) {
  response <- httr2::request(connection$url) %>%
    httr2::req_timeout(connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(
      token = connection$token,
      content = "project",
      format = "json"
    ) %>%
    httr2::req_perform()
  
  if (httr2::resp_is_error(response)) {
    stop("HTTP error ", httr2::resp_status(response), ": ", httr2::resp_body_string(response))
  }
  
  return(httr2::resp_body_json(response))
}

.redcap_get_metadata <- function(connection) {
  response <- httr2::request(connection$url) %>%
    httr2::req_timeout(connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(
      token = connection$token,
      content = "metadata",
      format = "json"
    ) %>%
    httr2::req_perform()
  
  if (httr2::resp_is_error(response)) {
    stop("HTTP error ", httr2::resp_status(response))
  }
  
  metadata <- httr2::resp_body_json(response, simplifyVector = TRUE)
  return(tibble::as_tibble(metadata))
}

.redcap_get_all_records <- function(connection) {
  response <- httr2::request(connection$url) %>%
    httr2::req_timeout(connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(
      token = connection$token,
      content = "record",
      format = "json",
      type = "flat"
    ) %>%
    httr2::req_perform()
  
  if (httr2::resp_is_error(response)) {
    stop("HTTP error ", httr2::resp_status(response))
  }
  
  data <- httr2::resp_body_json(response, simplifyVector = TRUE)
  return(tibble::as_tibble(data))
}

#' Print Method for REDCap Projects
#'
#' @param x A redcap_project object
#' @param ... Additional arguments (unused)
#'
#' @export
print.redcap_project <- function(x, ...) {
  x$info()
  invisible(x)
}