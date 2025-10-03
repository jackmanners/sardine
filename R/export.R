#' Export Records from REDCap
#'
#' @description
#' Exports records from a REDCap project. This function provides flexible
#' options for filtering and formatting the exported data.
#'
#' @param connection A redcap_connection object created by \code{\link{redcap_connection}}
#' @param records Character vector of specific record IDs to export (optional)
#' @param fields Character vector of specific field names to export (optional)
#' @param forms Character vector of specific form names to export (optional)
#' @param events Character vector of specific event names to export (optional)
#' @param raw_or_label Character string. Export raw coded values ("raw") or 
#'   labels ("label"). Default is "raw"
#' @param raw_or_label_headers Character string. Export raw field names ("raw") or 
#'   labels ("label") as column headers. Default is "raw"
#' @param export_checkbox_labels Logical. Export checkbox labels instead of 
#'   raw values. Default is FALSE
#' @param return_format Character string. Format for returned data ("json", "csv", "xml"). 
#'   Default is "json"
#' @param export_survey_fields Logical. Export survey identifier and timestamp fields. 
#'   Default is FALSE
#' @param export_data_access_groups Logical. Export data access group field. 
#'   Default is FALSE
#' @param filter_logic Character string. Logic string to filter records (optional)
#'
#' @return A tibble containing the exported records
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export all records
#' all_data <- redcap_export_records(conn)
#'
#' # Export specific records
#' subset_data <- redcap_export_records(
#'   conn,
#'   records = c("001", "002", "003")
#' )
#'
#' # Export specific fields
#' field_data <- redcap_export_records(
#'   conn,
#'   fields = c("record_id", "age", "gender")
#' )
#'
#' # Export with labels
#' labeled_data <- redcap_export_records(
#'   conn,
#'   raw_or_label = "label"
#' )
#' }
#'
#' @export
redcap_export_records <- function(connection,
                                  records = NULL,
                                  fields = NULL,
                                  forms = NULL,
                                  events = NULL,
                                  raw_or_label = "raw",
                                  raw_or_label_headers = "raw",
                                  export_checkbox_labels = FALSE,
                                  return_format = "json",
                                  export_survey_fields = FALSE,
                                  export_data_access_groups = FALSE,
                                  filter_logic = NULL) {
  
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  # Validate parameters
  if (!raw_or_label %in% c("raw", "label")) {
    rlang::abort("raw_or_label must be either 'raw' or 'label'")
  }
  
  if (!raw_or_label_headers %in% c("raw", "label")) {
    rlang::abort("raw_or_label_headers must be either 'raw' or 'label'")
  }
  
  if (!return_format %in% c("json", "csv", "xml")) {
    rlang::abort("return_format must be 'json', 'csv', or 'xml'")
  }
  
  # Prepare request parameters
  params <- list(
    content = "record",
    format = return_format,
    type = "flat",
    rawOrLabel = raw_or_label,
    rawOrLabelHeaders = raw_or_label_headers,
    exportCheckboxLabel = export_checkbox_labels,
    exportSurveyFields = export_survey_fields,
    exportDataAccessGroups = export_data_access_groups
  )
  
  # Add optional parameters if provided
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
  
  if (!is.null(forms)) {
    if (!is.character(forms)) {
      rlang::abort("forms must be a character vector")
    }
    params$forms <- paste(forms, collapse = ",")
  }
  
  if (!is.null(events)) {
    if (!is.character(events)) {
      rlang::abort("events must be a character vector")
    }
    params$events <- paste(events, collapse = ",")
  }
  
  if (!is.null(filter_logic)) {
    if (!is.character(filter_logic) || length(filter_logic) != 1) {
      rlang::abort("filter_logic must be a single character string")
    }
    params$filterLogic <- filter_logic
  }
  
  tryCatch({
    cli::cli_alert_info("Exporting records from REDCap...")
    
    # Make the API request
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    # Parse the response based on format
    if (return_format == "json") {
      data_raw <- httr2::resp_body_json(response)
      
      if (length(data_raw) == 0) {
        cli::cli_alert_warning("No records found matching the criteria")
        return(tibble::tibble())
      }
      
      # Convert to tibble
      data <- purrr::map_dfr(data_raw, ~ {
        # Handle empty values and convert to character
        tibble::as_tibble(lapply(.x, function(y) if(is.null(y)) NA_character_ else as.character(y)))
      })
      
    } else if (return_format == "csv") {
      csv_text <- httr2::resp_body_string(response)
      # Parse CSV manually to avoid additional dependencies
      data <- utils::read.csv(text = csv_text, stringsAsFactors = FALSE)
      data <- tibble::as_tibble(data)
      
    } else {
      # XML format - convert to data frame
      xml_text <- httr2::resp_body_string(response)
      rlang::abort("XML format parsing not yet implemented")
    }
    
    cli::cli_alert_success("Successfully exported {nrow(data)} records")
    
    return(data)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export records: {e$message}")
    rlang::abort(e$message)
  })
}