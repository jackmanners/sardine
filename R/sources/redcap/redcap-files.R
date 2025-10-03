#' Export File from REDCap
#'
#' @description
#' Exports a file from a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param record Character string. Record ID containing the file
#' @param field Character string. Field name containing the file
#' @param event Character string. Event name for longitudinal projects (optional)
#' @param repeat_instance Numeric. Repeat instance number for repeating forms (optional)
#'
#' @return Raw file data
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export a file
#' file_data <- redcap_export_file(conn, "001", "upload_field")
#' 
#' # Save to file
#' writeBin(file_data, "downloaded_file.pdf")
#' }
#'
#' @export
redcap_export_file <- function(connection, record, field, event = NULL, repeat_instance = NULL) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(record) || missing(field)) {
    rlang::abort("Both record and field are required")
  }
  
  if (!is.character(record) || length(record) != 1) {
    rlang::abort("record must be a single character string")
  }
  
  if (!is.character(field) || length(field) != 1) {
    rlang::abort("field must be a single character string")
  }
  
  params <- list(
    content = "file",
    action = "export",
    record = record,
    field = field
  )
  
  if (!is.null(event)) {
    if (!is.character(event) || length(event) != 1) {
      rlang::abort("event must be a single character string")
    }
    params$event <- event
  }
  
  if (!is.null(repeat_instance)) {
    if (!is.numeric(repeat_instance) || length(repeat_instance) != 1) {
      rlang::abort("repeat_instance must be a single numeric value")
    }
    params$repeat_instance <- repeat_instance
  }
  
  tryCatch({
    cli::cli_alert_info("Exporting file from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    # For files, we want the raw binary data
    result <- httr2::resp_body_raw(response)
    
    cli::cli_alert_success("Successfully exported file")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export file: {e$message}")
    rlang::abort(e$message)
  })
}

#' Import File to REDCap
#'
#' @description
#' Imports a file to a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param record Character string. Record ID to associate with the file
#' @param field Character string. Field name to upload the file to
#' @param file_path Character string. Path to the file to upload
#' @param event Character string. Event name for longitudinal projects (optional)
#' @param repeat_instance Numeric. Repeat instance number for repeating forms (optional)
#'
#' @return Success message
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Import a file
#' result <- redcap_import_file(conn, "001", "upload_field", "path/to/file.pdf")
#' }
#'
#' @export
redcap_import_file <- function(connection, record, field, file_path, event = NULL, repeat_instance = NULL) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(record) || missing(field) || missing(file_path)) {
    rlang::abort("record, field, and file_path are all required")
  }
  
  if (!is.character(record) || length(record) != 1) {
    rlang::abort("record must be a single character string")
  }
  
  if (!is.character(field) || length(field) != 1) {
    rlang::abort("field must be a single character string")
  }
  
  if (!is.character(file_path) || length(file_path) != 1) {
    rlang::abort("file_path must be a single character string")
  }
  
  if (!file.exists(file_path)) {
    rlang::abort(glue::glue("File does not exist: {file_path}"))
  }
  
  params <- list(
    content = "file",
    action = "import",
    record = record,
    field = field,
    file = file_path  # Will handle file upload in the request
  )
  
  if (!is.null(event)) {
    if (!is.character(event) || length(event) != 1) {
      rlang::abort("event must be a single character string")
    }
    params$event <- event
  }
  
  if (!is.null(repeat_instance)) {
    if (!is.numeric(repeat_instance) || length(repeat_instance) != 1) {
      rlang::abort("repeat_instance must be a single numeric value")
    }
    params$repeat_instance <- repeat_instance
  }
  
  tryCatch({
    cli::cli_alert_info("Importing file to REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully imported file")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to import file: {e$message}")
    rlang::abort(e$message)
  })
}

#' Delete File from REDCap
#'
#' @description
#' Deletes a file from a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param record Character string. Record ID containing the file
#' @param field Character string. Field name containing the file
#' @param event Character string. Event name for longitudinal projects (optional)
#' @param repeat_instance Numeric. Repeat instance number for repeating forms (optional)
#'
#' @return Success message
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Delete a file
#' result <- redcap_delete_file(conn, "001", "upload_field")
#' }
#'
#' @export
redcap_delete_file <- function(connection, record, field, event = NULL, repeat_instance = NULL) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(record) || missing(field)) {
    rlang::abort("Both record and field are required")
  }
  
  if (!is.character(record) || length(record) != 1) {
    rlang::abort("record must be a single character string")
  }
  
  if (!is.character(field) || length(field) != 1) {
    rlang::abort("field must be a single character string")
  }
  
  params <- list(
    content = "file",
    action = "delete",
    record = record,
    field = field
  )
  
  if (!is.null(event)) {
    if (!is.character(event) || length(event) != 1) {
      rlang::abort("event must be a single character string")
    }
    params$event <- event
  }
  
  if (!is.null(repeat_instance)) {
    if (!is.numeric(repeat_instance) || length(repeat_instance) != 1) {
      rlang::abort("repeat_instance must be a single numeric value")
    }
    params$repeat_instance <- repeat_instance
  }
  
  tryCatch({
    cli::cli_alert_info("Deleting file from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully deleted file")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to delete file: {e$message}")
    rlang::abort(e$message)
  })
}