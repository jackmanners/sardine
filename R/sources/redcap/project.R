#' REDCap Project Object
#'
#' @description
#' Creates a REDCap project object that provides an object-oriented interface
#' to a REDCap project. The project object tests connectivity on creation,
#' caches the full dataset, and provides methods for data access and manipulation.
#'
#' @param url Character string. The REDCap API URL (e.g., "https://redcap.example.edu/api/")
#' @param token Character string. Your REDCap API token
#' @param ssl_verify Logical. Whether to verify SSL certificates (default: TRUE)
#' @param timeout Numeric. Request timeout in seconds (default: 30)
#'
#' @return A redcap_project object with cached data and methods
#'
#' @section Methods:
#' \describe{
#'   \item{\code{$data}}{Full cached dataset from the project}
#'   \item{\code{$metadata}}{Project metadata (field definitions)}
#'   \item{\code{$project_info}}{Basic project information}
#'   \item{\code{$refresh()}}{Refresh cached data from REDCap}
#'   \item{\code{$info()}}{Display project summary}
#' }
#'
#' @section Usage with other functions:
#' \describe{
#'   \item{\code{export_records(project, fields)}}{Export specific fields}
#'   \item{\code{import_records(project, data)}}{Import data to REDCap}
#'   \item{\code{export_instruments(project)}}{Export form information}
#' }
#'
#' @examples
#' \dontrun{
#' # Create project (tests connection and caches data)
#' project <- redcap_project(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Access full data
#' all_data <- project$data
#'
#' # Get specific fields
#' subset_data <- export_records(project, fields = c("record_id", "age", "gender"))
#'
#' # View project info
#' project$info()
#' }
#'
#' @export
redcap_project <- function(url, token, ssl_verify = TRUE, timeout = 30) {
  
  # Validate inputs
  if (missing(url) || is.null(url) || url == "") {
    stop("REDCap URL is required")
  }
  
  if (missing(token) || is.null(token) || token == "") {
    stop("REDCap API token is required")
  }
  
  # Validate REDCap-specific URL format
  if (!stringr::str_detect(url, "/api/?$")) {
    cli::cli_alert_warning("URL should end with '/api/' for REDCap projects")
    # Auto-correct common mistakes
    if (!stringr::str_detect(url, "/api")) {
      url <- paste0(stringr::str_remove(url, "/$"), "/api/")
      cli::cli_alert_info("Auto-corrected URL to: {url}")
    }
  }
  
  cli::cli_alert_info("Creating REDCap project connection...")
  
  # Create base connection for API calls
  connection <- list(
    url = url,
    token = token,
    ssl_verify = ssl_verify,
    timeout = timeout
  )
  
  # Test connection and get project info
  cli::cli_alert_info("Testing connection and fetching project information...")
  
  tryCatch({
    project_info <- .redcap_get_project_info(connection)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to connect to REDCap project")
    cli::cli_alert_danger("Error: {e$message}")
    stop("REDCap project creation failed. Check your URL and token.", call. = FALSE)
  })
  
  cli::cli_alert_success("Connected to REDCap project: {project_info$project_title}")
  
  # Get metadata
  cli::cli_alert_info("Fetching project metadata...")
  tryCatch({
    metadata <- .redcap_get_metadata(connection)
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch metadata: {e$message}")
    metadata <- NULL
  })
  
  # Get full dataset
  cli::cli_alert_info("Caching full dataset...")
  tryCatch({
    data <- .redcap_get_all_records(connection)
    cli::cli_alert_success("Cached {nrow(data)} records with {ncol(data)} fields")
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch data: {e$message}")
    data <- NULL
  })
  
  # Create project object
  project <- list(
    # Connection details (private)
    .connection = connection,
    .created_at = Sys.time(),
    
    # Cached data (public access)
    data = data,
    metadata = metadata,
    project_info = project_info,
    
    # Methods
    refresh = function() {
      cli::cli_alert_info("Refreshing data from REDCap...")
      tryCatch({
        project$data <<- .redcap_get_all_records(connection)
        cli::cli_alert_success("Data refreshed: {nrow(project$data)} records")
        return(invisible(project$data))
      }, error = function(e) {
        cli::cli_alert_danger("Failed to refresh data: {e$message}")
        return(invisible(NULL))
      })
    },
    
    info = function() {
      cat("REDCap Project\n")
      cat("==============\n\n")
      cat("Title:", project_info$project_title, "\n")
      cat("URL:", stringr::str_remove(url, "/api/?$"), "\n")
      cat("Created:", format(.created_at, "%Y-%m-%d %H:%M:%S"), "\n")
      
      if (!is.null(data)) {
        cat("Cached Data:", nrow(data), "records,", ncol(data), "fields\n")
      } else {
        cat("Cached Data: None\n")
      }
      
      if (!is.null(metadata)) {
        cat("Metadata:", nrow(metadata), "fields defined\n")
      }
      
      cat("\nAccess data with: project$data\n")
      cat("Refresh data with: project$refresh()\n")
      cat("Export specific fields with: export_records(project, fields = c(...))\n")
      
      invisible(project)
    }
  )
  
  # Set class
  class(project) <- c("redcap_project", "sardine_project")
  
  return(project)
}

#' Create REDCap Project from Environment Variables
#'
#' @description
#' Creates a REDCap project using environment variables for configuration.
#' This is a convenience wrapper around redcap_project().
#'
#' @param env_prefix Character string. Prefix for environment variables (default: "REDCAP")
#'
#' @return A redcap_project object
#'
#' @examples
#' \dontrun{
#' # Using default REDCAP_URL and REDCAP_TOKEN
#' project <- redcap_project_from_env()
#'
#' # Using alternative environment variables
#' project <- redcap_project_from_env("REDCAP_2")
#' }
#'
#' @export
redcap_project_from_env <- function(env_prefix = "REDCAP") {
  
  url_var <- paste0(env_prefix, "_URL")
  token_var <- paste0(env_prefix, "_TOKEN")
  
  url <- Sys.getenv(url_var)
  token <- Sys.getenv(token_var)
  
  if (url == "" || token == "") {
    cli::cli_alert_danger("Environment variables {url_var} and {token_var} must be set")
    cli::cli_alert_info("Use load_env() to load from .env file")
    stop("Missing required environment variables", call. = FALSE)
  }
  
  return(redcap_project(url = url, token = token))
}

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