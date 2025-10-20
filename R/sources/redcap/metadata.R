#' Get REDCap Project Metadata
#'
#' @description
#' Retrieves the metadata (data dictionary) for a REDCap project, including
#' field names, types, labels, and validation rules.
#'
#' @param connection A redcap_connection object created by \code{\link{redcap_connection}}
#' @param fields Character vector of specific field names to retrieve metadata for (optional)
#' @param forms Character vector of specific form names to retrieve metadata for (optional)
#'
#' @return A tibble containing the project metadata
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Get all metadata
#' metadata <- redcap_metadata(conn)
#'
#' # Get metadata for specific fields
#' field_metadata <- redcap_metadata(
#'   conn,
#'   fields = c("record_id", "age", "gender")
#' )
#'
#' # Get metadata for specific forms
#' form_metadata <- redcap_metadata(
#'   conn,
#'   forms = c("demographics", "medical_history")
#' )
#' }
#'
#' @export
redcap_metadata <- function(connection, fields = NULL, forms = NULL) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  # Prepare request parameters
  params <- list(
    content = "metadata",
    format = "json"
  )
  
  # Add optional parameters if provided
  if (!is.null(fields)) {
    if (!is.character(fields)) {
      rlang::abort("fields must be a character vector")
    }
    params$fields <- paste(fields, collapse = ",")
  }
  
  if (!is.null(forms)) {
    if (!is.character(forms)) {
      rlang::abort("forms must be a character vector")
    }
    params$forms <- paste(forms, collapse = ",")
  }
  
  tryCatch({
    cli::cli_alert_info("Retrieving project metadata...")
    
    # Make the API request
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    # Parse the response
    metadata_raw <- httr2::resp_body_json(response)
    
    if (length(metadata_raw) == 0) {
      cli::cli_alert_warning("No metadata found")
      return(tibble::tibble())
    }
    
    # Convert to tibble
    metadata <- purrr::map_dfr(metadata_raw, ~ {
      tibble::as_tibble(lapply(.x, function(y) if(is.null(y)) NA_character_ else as.character(y)))
    })
    
    cli::cli_alert_success("Successfully retrieved metadata for {nrow(metadata)} fields")
    
    return(metadata)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to retrieve metadata: {e$message}")
    rlang::abort(e$message)
  })
}

#' Get REDCap Project Information
#'
#' @description
#' Retrieves basic information about a REDCap project, including project title,
#' creation date, production status, and other project-level details.
#'
#' @param connection A redcap_connection object created by \code{\link{redcap_connection}}
#'
#' @return A list containing project information
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Get project information
#' project_info <- redcap_project_info(conn)
#' print(project_info$project_title)
#' }
#'
#' @export
redcap_project_info <- function(connection) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  tryCatch({
    cli::cli_alert_info("Retrieving project information...")
    
    # Make the API request
    response <- .redcap_request(
      connection = connection,
      content = "project",
      format = "json"
    )
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    # Parse the response
    project_info <- httr2::resp_body_json(response)
    
    cli::cli_alert_success("Successfully retrieved project information")
    
    return(project_info)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to retrieve project information: {e$message}")
    rlang::abort(e$message)
  })
}

#' Get REDCap Field Names
#'
#' @description
#' Retrieves a list of all field names in a REDCap project.
#'
#' @param connection A redcap_connection object created by \code{\link{redcap_connection}}
#'
#' @return A character vector of field names
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Get all field names
#' field_names <- redcap_field_names(conn)
#' }
#'
#' @export
redcap_field_names <- function(connection) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  tryCatch({
    cli::cli_alert_info("Retrieving field names...")
    
    # Make the API request
    response <- .redcap_request(
      connection = connection,
      content = "exportFieldNames",
      format = "json"
    )
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    # Parse the response
    field_data <- httr2::resp_body_json(response)
    
    # Extract field names
    field_names <- purrr::map_chr(field_data, ~ .x$export_field_name)
    
    cli::cli_alert_success("Successfully retrieved {length(field_names)} field names")
    
    return(field_names)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to retrieve field names: {e$message}")
    rlang::abort(e$message)
  })
}