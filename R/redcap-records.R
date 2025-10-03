#' Import Records to REDCap
#'
#' @description
#' Imports records to a REDCap project. Can be used to create new records or
#' update existing ones.
#'
#' @param connection A redcap_connection object
#' @param data A data.frame or list containing the records to import
#' @param overwrite_behavior Character string. How to handle existing records:
#'   "normal" (default), "overwrite"
#' @param format_data Character string. Format of the data: "json" (default), "csv", "xml"
#' @param type Character string. Type of records: "flat" (default), "eav"
#' @param date_format Character string. Format for dates: "YMD" (default), "MDY", "DMY"
#' @param csv_delimiter Character string. Delimiter for CSV format (default: ",")
#' @param return_content Character string. What to return: "count" (default), "ids", "auto_ids"
#' @param return_format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return The response from REDCap (count of imported records, IDs, etc.)
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Import a single record
#' new_record <- data.frame(
#'   record_id = "001",
#'   first_name = "John",
#'   last_name = "Doe",
#'   age = 30
#' )
#' 
#' result <- redcap_import_records(conn, new_record)
#' }
#'
#' @export
redcap_import_records <- function(connection,
                                  data,
                                  overwrite_behavior = "normal",
                                  format_data = "json",
                                  type = "flat",
                                  date_format = "YMD",
                                  csv_delimiter = ",",
                                  return_content = "count",
                                  return_format = "json") {
  
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(data) || is.null(data)) {
    rlang::abort("data is required")
  }
  
  # Convert data to appropriate format
  if (is.data.frame(data)) {
    if (format_data == "json") {
      data_string <- jsonlite::toJSON(data, auto_unbox = TRUE)
    } else if (format_data == "csv") {
      temp_file <- tempfile(fileext = ".csv")
      utils::write.csv(data, temp_file, row.names = FALSE)
      data_string <- paste(readLines(temp_file), collapse = "\n")
      unlink(temp_file)
    } else {
      rlang::abort("XML format not yet supported for data import")
    }
  } else if (is.list(data)) {
    data_string <- jsonlite::toJSON(data, auto_unbox = TRUE)
  } else if (is.character(data)) {
    data_string <- data
  } else {
    rlang::abort("data must be a data.frame, list, or character string")
  }
  
  # Prepare request parameters
  params <- list(
    content = "record",
    format = return_format,
    type = type,
    overwriteBehavior = overwrite_behavior,
    data = data_string,
    dateFormat = date_format,
    csvDelimiter = csv_delimiter,
    returnContent = return_content
  )
  
  tryCatch({
    cli::cli_alert_info("Importing records to REDCap...")
    
    # Make the API request
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    # Parse the response
    if (return_format == "json") {
      result <- httr2::resp_body_json(response)
    } else {
      result <- httr2::resp_body_string(response)
    }
    
    cli::cli_alert_success("Successfully imported records")
    
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to import records: {e$message}")
    rlang::abort(e$message)
  })
}

#' Delete Records from REDCap
#'
#' @description
#' Deletes records from a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param records Character vector of record IDs to delete
#' @param arm Numeric. Arm number for longitudinal projects (optional)
#'
#' @return The number of records deleted
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Delete specific records
#' result <- redcap_delete_records(conn, c("001", "002"))
#' }
#'
#' @export
redcap_delete_records <- function(connection, records, arm = NULL) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(records) || is.null(records)) {
    rlang::abort("records is required")
  }
  
  if (!is.character(records)) {
    rlang::abort("records must be a character vector")
  }
  
  # Prepare request parameters
  params <- list(
    content = "record",
    action = "delete",
    records = paste(records, collapse = ",")
  )
  
  if (!is.null(arm)) {
    if (!is.numeric(arm) || length(arm) != 1) {
      rlang::abort("arm must be a single numeric value")
    }
    params$arm <- arm
  }
  
  tryCatch({
    cli::cli_alert_info("Deleting {length(records)} record(s) from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully deleted records")
    
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to delete records: {e$message}")
    rlang::abort(e$message)
  })
}

#' Rename Record in REDCap
#'
#' @description
#' Renames a record ID in a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param old_id Character string. Current record ID
#' @param new_id Character string. New record ID
#' @param arm Numeric. Arm number for longitudinal projects (optional)
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
#' # Rename a record
#' result <- redcap_rename_record(conn, "old_id", "new_id")
#' }
#'
#' @export
redcap_rename_record <- function(connection, old_id, new_id, arm = NULL) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(old_id) || missing(new_id)) {
    rlang::abort("Both old_id and new_id are required")
  }
  
  if (!is.character(old_id) || length(old_id) != 1) {
    rlang::abort("old_id must be a single character string")
  }
  
  if (!is.character(new_id) || length(new_id) != 1) {
    rlang::abort("new_id must be a single character string")
  }
  
  # Prepare request parameters
  params <- list(
    content = "record",
    action = "rename",
    record = old_id,
    new_record_name = new_id
  )
  
  if (!is.null(arm)) {
    if (!is.numeric(arm) || length(arm) != 1) {
      rlang::abort("arm must be a single numeric value")
    }
    params$arm <- arm
  }
  
  tryCatch({
    cli::cli_alert_info("Renaming record '{old_id}' to '{new_id}'...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully renamed record")
    
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to rename record: {e$message}")
    rlang::abort(e$message)
  })
}