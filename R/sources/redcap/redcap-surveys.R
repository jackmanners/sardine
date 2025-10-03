#' Export Survey Link
#'
#' @description
#' Exports a survey link for a specific record and instrument.
#'
#' @param connection A redcap_connection object
#' @param record Character string. Record ID for the survey link
#' @param instrument Character string. Instrument name for the survey
#' @param event Character string. Event name for longitudinal projects (optional)
#' @param repeat_instance Numeric. Repeat instance for repeating instruments (optional)
#'
#' @return Character string containing the survey URL
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Get survey link for a record
#' survey_url <- redcap_export_survey_link(conn, "001", "baseline_survey")
#' print(survey_url)
#' }
#'
#' @export
redcap_export_survey_link <- function(connection, record, instrument, event = NULL, repeat_instance = NULL) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(record) || missing(instrument)) {
    rlang::abort("Both record and instrument are required")
  }
  
  if (!is.character(record) || length(record) != 1) {
    rlang::abort("record must be a single character string")
  }
  
  if (!is.character(instrument) || length(instrument) != 1) {
    rlang::abort("instrument must be a single character string")
  }
  
  params <- list(
    content = "surveyLink",
    format = "json",
    record = record,
    instrument = instrument
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
    cli::cli_alert_info("Generating survey link...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully generated survey link")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to generate survey link: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export Survey Queue Link
#'
#' @description
#' Exports a survey queue link for a specific record.
#'
#' @param connection A redcap_connection object
#' @param record Character string. Record ID for the survey queue link
#'
#' @return Character string containing the survey queue URL
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Get survey queue link for a record
#' queue_url <- redcap_export_survey_queue_link(conn, "001")
#' print(queue_url)
#' }
#'
#' @export
redcap_export_survey_queue_link <- function(connection, record) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(record)) {
    rlang::abort("record is required")
  }
  
  if (!is.character(record) || length(record) != 1) {
    rlang::abort("record must be a single character string")
  }
  
  params <- list(
    content = "surveyQueueLink",
    format = "json",
    record = record
  )
  
  tryCatch({
    cli::cli_alert_info("Generating survey queue link...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully generated survey queue link")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to generate survey queue link: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export Survey Return Code
#'
#' @description
#' Exports a survey return code for a specific record and instrument.
#'
#' @param connection A redcap_connection object
#' @param record Character string. Record ID for the survey return code
#' @param instrument Character string. Instrument name for the survey
#' @param event Character string. Event name for longitudinal projects (optional)
#' @param repeat_instance Numeric. Repeat instance for repeating instruments (optional)
#'
#' @return Character string containing the survey return code
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Get survey return code for a record
#' return_code <- redcap_export_survey_return_code(conn, "001", "baseline_survey")
#' print(return_code)
#' }
#'
#' @export
redcap_export_survey_return_code <- function(connection, record, instrument, event = NULL, repeat_instance = NULL) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(record) || missing(instrument)) {
    rlang::abort("Both record and instrument are required")
  }
  
  if (!is.character(record) || length(record) != 1) {
    rlang::abort("record must be a single character string")
  }
  
  if (!is.character(instrument) || length(instrument) != 1) {
    rlang::abort("instrument must be a single character string")
  }
  
  params <- list(
    content = "surveyReturnCode",
    format = "json",
    record = record,
    instrument = instrument
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
    cli::cli_alert_info("Generating survey return code...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully generated survey return code")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to generate survey return code: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export Survey Participants
#'
#' @description
#' Exports the survey participants for a specific instrument.
#'
#' @param connection A redcap_connection object
#' @param instrument Character string. Instrument name for the survey
#' @param event Character string. Event name for longitudinal projects (optional)
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing survey participant information
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export survey participants
#' participants <- redcap_export_survey_participants(conn, "baseline_survey")
#' }
#'
#' @export
redcap_export_survey_participants <- function(connection, instrument, event = NULL, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(instrument)) {
    rlang::abort("instrument is required")
  }
  
  if (!is.character(instrument) || length(instrument) != 1) {
    rlang::abort("instrument must be a single character string")
  }
  
  params <- list(
    content = "participantList",
    format = format,
    instrument = instrument
  )
  
  if (!is.null(event)) {
    if (!is.character(event) || length(event) != 1) {
      rlang::abort("event must be a single character string")
    }
    params$event <- event
  }
  
  tryCatch({
    cli::cli_alert_info("Exporting survey participants...")
    
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
    
    cli::cli_alert_success("Successfully exported survey participants")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export survey participants: {e$message}")
    rlang::abort(e$message)
  })
}