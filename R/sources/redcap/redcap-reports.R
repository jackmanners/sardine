#' Export REDCap Reports
#'
#' @description
#' Exports a report from REDCap using the report ID.
#'
#' @param connection A redcap_connection object
#' @param report_id Character string or numeric. The report ID to export
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#' @param raw_or_label Character string. Export raw coded values ("raw") or 
#'   labels ("label"). Default is "raw"
#' @param raw_or_label_headers Character string. Export raw field names ("raw") or 
#'   labels ("label") as column headers. Default is "raw"
#' @param export_checkbox_labels Logical. Export checkbox labels instead of 
#'   raw values. Default is FALSE
#'
#' @return A tibble containing the report data
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export a specific report
#' report_data <- redcap_export_reports(conn, report_id = "12345")
#' }
#'
#' @export
redcap_export_reports <- function(connection, 
                                  report_id,
                                  format = "json",
                                  raw_or_label = "raw",
                                  raw_or_label_headers = "raw",
                                  export_checkbox_labels = FALSE) {
  
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(report_id)) {
    rlang::abort("report_id is required")
  }
  
  if (!is.character(report_id) && !is.numeric(report_id)) {
    rlang::abort("report_id must be a character string or numeric")
  }
  
  params <- list(
    content = "report",
    format = format,
    report_id = as.character(report_id),
    rawOrLabel = raw_or_label,
    rawOrLabelHeaders = raw_or_label_headers,
    exportCheckboxLabel = export_checkbox_labels
  )
  
  tryCatch({
    cli::cli_alert_info("Exporting report from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    if (format == "json") {
      result_raw <- httr2::resp_body_json(response)
      if (length(result_raw) == 0) {
        return(tibble::tibble())
      }
      result <- purrr::map_dfr(result_raw, ~ {
        tibble::as_tibble(lapply(.x, function(y) if(is.null(y)) NA_character_ else as.character(y)))
      })
    } else if (format == "csv") {
      csv_text <- httr2::resp_body_string(response)
      data <- utils::read.csv(text = csv_text, stringsAsFactors = FALSE)
      result <- tibble::as_tibble(data)
    } else {
      result <- httr2::resp_body_string(response)
    }
    
    cli::cli_alert_success("Successfully exported report")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export report: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export REDCap Logging
#'
#' @description
#' Exports the logging information from a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#' @param log_type Character string. Type of log entries: "export", "manage", "user", "record", "record_add", "record_edit", "record_delete", "lock_record", "page_view"
#' @param user Character string. Username to filter by (optional)
#' @param record Character string. Record ID to filter by (optional)
#' @param dag Character string. Data Access Group to filter by (optional)
#' @param begin_time Character string. Begin time for log entries (YYYY-MM-DD HH:MM format, optional)
#' @param end_time Character string. End time for log entries (YYYY-MM-DD HH:MM format, optional)
#'
#' @return A tibble containing logging information
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export all logging
#' logs <- redcap_export_logging(conn)
#' 
#' # Export logs for specific user
#' user_logs <- redcap_export_logging(conn, user = "john.doe")
#' 
#' # Export logs for specific time period
#' recent_logs <- redcap_export_logging(
#'   conn, 
#'   begin_time = "2023-01-01 00:00", 
#'   end_time = "2023-12-31 23:59"
#' )
#' }
#'
#' @export
redcap_export_logging <- function(connection,
                                  format = "json",
                                  log_type = NULL,
                                  user = NULL,
                                  record = NULL,
                                  dag = NULL,
                                  begin_time = NULL,
                                  end_time = NULL) {
  
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "log",
    format = format
  )
  
  if (!is.null(log_type)) {
    valid_types <- c("export", "manage", "user", "record", "record_add", "record_edit", 
                     "record_delete", "lock_record", "page_view")
    if (!log_type %in% valid_types) {
      rlang::abort(glue::glue("log_type must be one of: {paste(valid_types, collapse = ', ')}"))
    }
    params$logtype <- log_type
  }
  
  if (!is.null(user)) {
    if (!is.character(user) || length(user) != 1) {
      rlang::abort("user must be a single character string")
    }
    params$user <- user
  }
  
  if (!is.null(record)) {
    if (!is.character(record) || length(record) != 1) {
      rlang::abort("record must be a single character string")
    }
    params$record <- record
  }
  
  if (!is.null(dag)) {
    if (!is.character(dag) || length(dag) != 1) {
      rlang::abort("dag must be a single character string")
    }
    params$dag <- dag
  }
  
  if (!is.null(begin_time)) {
    if (!is.character(begin_time) || length(begin_time) != 1) {
      rlang::abort("begin_time must be a single character string")
    }
    params$beginTime <- begin_time
  }
  
  if (!is.null(end_time)) {
    if (!is.character(end_time) || length(end_time) != 1) {
      rlang::abort("end_time must be a single character string")
    }
    params$endTime <- end_time
  }
  
  tryCatch({
    cli::cli_alert_info("Exporting logging from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    if (format == "json") {
      result_raw <- httr2::resp_body_json(response)
      if (length(result_raw) == 0) {
        return(tibble::tibble())
      }
      result <- purrr::map_dfr(result_raw, ~ {
        tibble::as_tibble(lapply(.x, function(y) if(is.null(y)) NA_character_ else as.character(y)))
      })
    } else if (format == "csv") {
      csv_text <- httr2::resp_body_string(response)
      data <- utils::read.csv(text = csv_text, stringsAsFactors = FALSE)
      result <- tibble::as_tibble(data)
    } else {
      result <- httr2::resp_body_string(response)
    }
    
    cli::cli_alert_success("Successfully exported logging")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export logging: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export REDCap Version
#'
#' @description
#' Exports the REDCap version information.
#'
#' @param connection A redcap_connection object
#'
#' @return Character string containing the REDCap version
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Get REDCap version
#' version <- redcap_export_version(conn)
#' print(version)
#' }
#'
#' @export
redcap_export_version <- function(connection) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "version"
  )
  
  tryCatch({
    cli::cli_alert_info("Getting REDCap version...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully retrieved REDCap version: {result}")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to get REDCap version: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export Complete Project as XML
#'
#' @description
#' Exports the complete project definition as XML, including metadata, events, arms, etc.
#'
#' @param connection A redcap_connection object
#' @param return_metadata_only Logical. Whether to return only metadata (default: FALSE)
#' @param records Character vector. Specific record IDs to include (optional)
#' @param fields Character vector. Specific field names to include (optional)
#' @param events Character vector. Specific event names to include (optional)
#' @param export_survey_fields Logical. Whether to export survey fields (default: FALSE)
#' @param export_data_access_groups Logical. Whether to export DAG field (default: FALSE)
#' @param export_files Logical. Whether to export files (default: FALSE)
#'
#' @return Character string containing XML data
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export complete project as XML
#' xml_data <- redcap_export_project_xml(conn)
#' 
#' # Save to file
#' writeLines(xml_data, "project_backup.xml")
#' }
#'
#' @export
redcap_export_project_xml <- function(connection,
                                      return_metadata_only = FALSE,
                                      records = NULL,
                                      fields = NULL,
                                      events = NULL,
                                      export_survey_fields = FALSE,
                                      export_data_access_groups = FALSE,
                                      export_files = FALSE) {
  
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "project_xml",
    returnMetadataOnly = return_metadata_only,
    exportSurveyFields = export_survey_fields,
    exportDataAccessGroups = export_data_access_groups,
    exportFiles = export_files
  )
  
  if (!is.null(records)) {
    if (!is.character(records)) {
      rlang::abort("records must be a character vector")
    }
    params$records <- paste(records, collapse = ",")
  }
  
  if (!is.null(fields)) {
    if (!is.character(fields)) {
      rlang::abort("fields must be a character vector")
    }
    params$fields <- paste(fields, collapse = ",")
  }
  
  if (!is.null(events)) {
    if (!is.character(events)) {
      rlang::abort("events must be a character vector")
    }
    params$events <- paste(events, collapse = ",")
  }
  
  tryCatch({
    cli::cli_alert_info("Exporting project XML from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully exported project XML")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export project XML: {e$message}")
    rlang::abort(e$message)
  })
}