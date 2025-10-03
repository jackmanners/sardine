#' Export REDCap Instruments (Forms)
#'
#' @description
#' Exports the list of instruments (forms) in a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing instrument information
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export all instruments
#' instruments <- redcap_export_instruments(conn)
#' }
#'
#' @export
redcap_export_instruments <- function(connection, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "instrument",
    format = format
  )
  
  tryCatch({
    cli::cli_alert_info("Exporting instruments from REDCap...")
    
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
      result <- purrr::map_dfr(result_raw, ~ tibble::as_tibble(lapply(.x, as.character)))
    } else {
      result <- httr2::resp_body_string(response)
    }
    
    cli::cli_alert_success("Successfully exported instruments")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export instruments: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export Instrument Event Mappings
#'
#' @description
#' Exports the instrument-event mappings for a longitudinal REDCap project.
#'
#' @param connection A redcap_connection object
#' @param arms Character vector of arm numbers to filter by (optional)
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing instrument-event mappings
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export all instrument-event mappings
#' mappings <- redcap_export_instrument_event_mappings(conn)
#' }
#'
#' @export
redcap_export_instrument_event_mappings <- function(connection, arms = NULL, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "formEventMapping",
    format = format
  )
  
  if (!is.null(arms)) {
    if (!is.character(arms) && !is.numeric(arms)) {
      rlang::abort("arms must be a character or numeric vector")
    }
    params$arms <- paste(arms, collapse = ",")
  }
  
  tryCatch({
    cli::cli_alert_info("Exporting instrument-event mappings from REDCap...")
    
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
      result <- purrr::map_dfr(result_raw, ~ tibble::as_tibble(lapply(.x, as.character)))
    } else {
      result <- httr2::resp_body_string(response)
    }
    
    cli::cli_alert_success("Successfully exported instrument-event mappings")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export instrument-event mappings: {e$message}")
    rlang::abort(e$message)
  })
}

#' Import Instrument Event Mappings
#'
#' @description
#' Imports instrument-event mappings to a longitudinal REDCap project.
#'
#' @param connection A redcap_connection object
#' @param data A data.frame containing instrument-event mappings
#'
#' @return Number of mappings imported
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Import mappings
#' mapping_data <- data.frame(
#'   arm_num = c(1, 1),
#'   unique_event_name = c("baseline_arm_1", "followup_arm_1"),
#'   form = c("demographics", "outcomes")
#' )
#' result <- redcap_import_instrument_event_mappings(conn, mapping_data)
#' }
#'
#' @export
redcap_import_instrument_event_mappings <- function(connection, data) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(data) || is.null(data)) {
    rlang::abort("data is required")
  }
  
  data_string <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  params <- list(
    content = "formEventMapping",
    action = "import",
    format = "json",
    data = data_string
  )
  
  tryCatch({
    cli::cli_alert_info("Importing instrument-event mappings to REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully imported instrument-event mappings")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to import instrument-event mappings: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export Instruments as PDF
#'
#' @description
#' Exports instruments (forms) from a REDCap project as a PDF.
#'
#' @param connection A redcap_connection object
#' @param record Character string. Specific record ID to use for PDF (optional)
#' @param event Character string. Specific event name for longitudinal projects (optional)
#' @param instrument Character string. Specific instrument to export (optional)
#' @param all_records Logical. Whether to include all records (default: FALSE)
#'
#' @return Raw PDF data
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export all instruments as PDF
#' pdf_data <- redcap_export_instruments_pdf(conn)
#' 
#' # Save to file
#' writeBin(pdf_data, "instruments.pdf")
#' }
#'
#' @export
redcap_export_instruments_pdf <- function(connection, 
                                          record = NULL, 
                                          event = NULL, 
                                          instrument = NULL, 
                                          all_records = FALSE) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "pdf",
    format = "json"
  )
  
  if (!is.null(record)) {
    if (!is.character(record) || length(record) != 1) {
      rlang::abort("record must be a single character string")
    }
    params$record <- record
  }
  
  if (!is.null(event)) {
    if (!is.character(event) || length(event) != 1) {
      rlang::abort("event must be a single character string")
    }
    params$event <- event
  }
  
  if (!is.null(instrument)) {
    if (!is.character(instrument) || length(instrument) != 1) {
      rlang::abort("instrument must be a single character string")
    }
    params$instrument <- instrument
  }
  
  if (all_records) {
    params$allRecords <- "true"
  }
  
  tryCatch({
    cli::cli_alert_info("Exporting instruments as PDF from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    # For PDF, we want the raw binary data
    result <- httr2::resp_body_raw(response)
    
    cli::cli_alert_success("Successfully exported instruments as PDF")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export instruments as PDF: {e$message}")
    rlang::abort(e$message)
  })
}